// faiss.cpp — FlatL2 + IDMap2 backend (atom IDs handled in Prolog, NO TRAINING)
#include <faiss/IndexFlat.h>
#include <faiss/IndexIDMap.h>
#include <SWI-Prolog.h>
#include <vector>
#include <cstdint>

extern "C" {

// ------------------------------------------------------------
// Wrapper structure
// ------------------------------------------------------------
struct FaissIndex {
    faiss::IndexIDMap2* index;
    int dim;
};

// ------------------------------------------------------------
// Helpers: Prolog list -> vectors
// ------------------------------------------------------------
static bool list_to_floats(term_t list, std::vector<float>& out) {
    term_t head = PL_new_term_ref();
    term_t tail = PL_copy_term_ref(list);

    while (PL_get_list(tail, head, tail)) {
        double v;
        if (!PL_get_float(head, &v)) {
            long iv;
            if (!PL_get_long(head, &iv))
                return false;
            v = (double)iv;
        }
        out.push_back((float)v);
    }
    return PL_get_nil(tail);
}

static bool list_to_ids(term_t list, std::vector<faiss::idx_t>& out) {
    term_t head = PL_new_term_ref();
    term_t tail = PL_copy_term_ref(list);

    while (PL_get_list(tail, head, tail)) {
        int64_t v;
        if (!PL_get_int64(head, &v))
            return false;
        out.push_back((faiss::idx_t)v);
    }
    return PL_get_nil(tail);
}

// ------------------------------------------------------------
// Raw FAISS construction (IVFFlat + IDMap2)
// ------------------------------------------------------------
static FaissIndex* faiss_create_raw(int dim) {
    faiss::IndexFlatL2* flat = new faiss::IndexFlatL2(dim);
    faiss::IndexIDMap2* idmap = new faiss::IndexIDMap2(flat);
    idmap->own_fields = true;

    FaissIndex* f = new FaissIndex;
    f->index = idmap;
    f->dim = dim;
    return f;
}

static void faiss_free_raw(FaissIndex* f) {
    delete f->index;
    delete f;
}

// ------------------------------------------------------------
// Foreign predicates (internal C endpoints)
// ------------------------------------------------------------

// faiss_create_c(+Dim, -Ptr)
foreign_t pl_faiss_create(term_t t_dim, term_t t_ptr) {
    int dim;
    if (!PL_get_integer(t_dim, &dim))
        return PL_type_error("integer", t_dim);

    FaissIndex* idx = faiss_create_raw(dim);
    return PL_unify_pointer(t_ptr, idx);
}

// faiss_free_c(+Ptr)
foreign_t pl_faiss_free(term_t t_ptr) {
    void* p;
    if (!PL_get_pointer(t_ptr, &p))
        return PL_type_error("pointer", t_ptr);

    faiss_free_raw((FaissIndex*)p);
    return true;
}

// faiss_add_with_ids_c(+Ptr,+Flat,+Ids)
foreign_t pl_faiss_add_with_ids(term_t t_ptr, term_t t_vecs, term_t t_ids) {
    void* p;
    if (!PL_get_pointer(t_ptr, &p))
        return PL_type_error("pointer", t_ptr);
    FaissIndex* f = (FaissIndex*)p;

    std::vector<float> flat;
    if (!list_to_floats(t_vecs, flat))
        return PL_type_error("float_list", t_vecs);

    std::vector<faiss::idx_t> ids;
    if (!list_to_ids(t_ids, ids))
        return PL_type_error("int_list", t_ids);

    if (flat.size() % f->dim != 0)
        return PL_domain_error("multiple_of_dimension", t_vecs);

    size_t n = flat.size() / f->dim;
    if (n != ids.size())
        return PL_domain_error("id_count_mismatch", t_ids);

    // Pure memory: no training, just add vectors with given IDs.
    f->index->add_with_ids(n, flat.data(), ids.data());
    return true;
}

// faiss_remove_ids_c(+Ptr,+Ids)
foreign_t pl_faiss_remove_ids(term_t t_ptr, term_t t_ids) {
    void* p;
    if (!PL_get_pointer(t_ptr, &p))
        return PL_type_error("pointer", t_ptr);
    FaissIndex* f = (FaissIndex*)p;

    std::vector<faiss::idx_t> ids;
    if (!list_to_ids(t_ids, ids))
        return PL_type_error("int_list", t_ids);

    if (ids.empty())
        return true;

    faiss::IDSelectorBatch sel(ids.size(), ids.data());
    f->index->remove_ids(sel);
    return true;
}

// faiss_search_c(+Ptr,+Query,+K,-ResultTerm)
foreign_t pl_faiss_search(term_t t_ptr,
                          term_t t_query,
                          term_t t_k,
                          term_t t_res)
{
    void* p;
    if (!PL_get_pointer(t_ptr, &p))
        return PL_type_error("pointer", t_ptr);
    FaissIndex* f = (FaissIndex*)p;

    std::vector<float> q;
    if (!list_to_floats(t_query, q))
        return PL_type_error("float_list", t_query);

    if (q.size() != (size_t)f->dim)
        return PL_domain_error("vector_dimension", t_query);

    int k;
    if (!PL_get_integer(t_k, &k))
        return PL_type_error("integer", t_k);

    std::vector<float> dist(k);
    std::vector<faiss::idx_t> ids(k);

    f->index->search(1, q.data(), k, dist.data(), ids.data());

    term_t tD = PL_new_term_ref();
    term_t tL = PL_new_term_ref();
    PL_put_nil(tD);
    PL_put_nil(tL);

    for (int i = k - 1; i >= 0; i--) {
        term_t h1 = PL_new_term_ref();
        PL_put_float(h1, dist[i]);
        PL_cons_list(tD, h1, tD);

        term_t h2 = PL_new_term_ref();
        PL_put_int64(h2, ids[i]);
        PL_cons_list(tL, h2, tL);
    }

    return PL_unify_term(
        t_res,
        PL_FUNCTOR, PL_new_functor(PL_new_atom("result"), 2),
        PL_TERM, tD,
        PL_TERM, tL
    );
}

// ------------------------------------------------------------
// Install
// ------------------------------------------------------------
void install_faisslib() {
    PL_register_foreign("faiss_create_c",        2, (pl_function_t)pl_faiss_create,        0);
    PL_register_foreign("faiss_free_c",          1, (pl_function_t)pl_faiss_free,          0);
    PL_register_foreign("faiss_add_with_ids_c",  3, (pl_function_t)pl_faiss_add_with_ids,  0);
    PL_register_foreign("faiss_remove_ids_c",    2, (pl_function_t)pl_faiss_remove_ids,    0);
    PL_register_foreign("faiss_search_c",        4, (pl_function_t)pl_faiss_search,        0);
}

} // extern "C"
