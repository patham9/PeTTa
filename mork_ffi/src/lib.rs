use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::{Mutex, OnceLock};
use mork_frontend::bytestring_parser::{Parser, ParserError, Context};
use mork::space::{Space, ParDataParser};
use mork_expr::{Expr, ExprZipper};

//S-Expression parsing:
static GLOBAL_SPACE: OnceLock<Mutex<Space>> = OnceLock::new();
fn get_space() -> &'static Mutex<Space> {
    GLOBAL_SPACE.get_or_init(|| Mutex::new(Space::new()))
}

//Reusable output buffer for each thread:
thread_local! {
    static OUTBUF: RefCell<Vec<u8>> = RefCell::new(Vec::with_capacity(64 * 1024));
}

//S-Expression parsing:
fn parse_sexpr(s: &Space, r: &[u8], buf: &mut [u8]) -> Result<(Expr, usize), ParserError> {
    let mut it = Context::new(r);
    let mut parser = ParDataParser::new(&s.sm);
    let mut ez = ExprZipper::new(Expr { ptr: buf.as_mut_ptr() });
    parser.sexpr(&mut it, &mut ez).map(|_| (Expr { ptr: buf.as_mut_ptr() }, ez.loc))
}

//Foreign Funcion Interface:
#[no_mangle]
pub extern "C" fn rust_mork(command: *const c_char, input: *const c_char) -> *mut c_char {
    if command.is_null() || input.is_null() {
        return std::ptr::null_mut();
    }
    let cmd = match unsafe { CStr::from_ptr(command) }.to_str() {
        Ok(s) => s,
        Err(_) => return CString::new("ERR: invalid command utf8").unwrap().into_raw(),
    };
    let inp = match unsafe { CStr::from_ptr(input) }.to_str() {
        Ok(s) => s,
        Err(_) => return CString::new("ERR: invalid input utf8").unwrap().into_raw(),
    };
    let mut result_ptr: *mut c_char = std::ptr::null_mut();
    OUTBUF.with(|cell| {
        let mut outbuf = cell.borrow_mut();
        outbuf.clear();
        let space = get_space();
        let mut out_static: &str = "OK";
        if cmd.eq_ignore_ascii_case("add-atoms") {
            let mut s = match space.lock() {
                Ok(g) => g,
                Err(_) => {
                    result_ptr = CString::new("ERR: space poisoned").unwrap().into_raw();
                    return;
                }
            };
            //Add S-exprs into the existing Space
            match s.add_all_sexpr(inp.as_bytes()) {
                Ok(_)  => out_static = "OK: loaded",
                Err(_) => out_static = "ERR: load failed",
            }
            result_ptr = CString::new(out_static).unwrap().into_raw();
        }
        else if cmd.eq_ignore_ascii_case("remove-atoms") {
            let mut s = match space.lock() {
                Ok(g) => g,
                Err(_) => {
                    result_ptr = CString::new("ERR: space poisoned").unwrap().into_raw();
                    return;
                }
            };
            //Remove S-exprs from the existing Space
            match s.remove_all_sexpr(inp.as_bytes()) {
                Ok(_)  => out_static = "OK: loaded",
                Err(_) => out_static = "ERR: load failed",
            }
            result_ptr = CString::new(out_static).unwrap().into_raw();
        }
        else if cmd.eq_ignore_ascii_case("mm2-exec") {
            let mut s = match space.lock() {
                Ok(g) => g,
                Err(_) => {
                    result_ptr = CString::new("ERR: space poisoned").unwrap().into_raw();
                    return;
                }
            };
            let num: usize = inp.trim().parse().unwrap_or(1);
            //Load S-exprs into the existing Space
            s.metta_calculus(num);
            result_ptr = CString::new("OK: executed").unwrap().into_raw();
        }
        else if cmd.eq_ignore_ascii_case("get-atoms") {
            let s = match space.lock() {
                Ok(g) => g,
                Err(_) => {
                    result_ptr = CString::new("ERR: space poisoned").unwrap().into_raw();
                    return;
                }
            };
            if let Err(_) = s.dump_all_sexpr(&mut *outbuf) {
                result_ptr = CString::new("ERR: dump failed").unwrap().into_raw();
                return;
            }
            let text = unsafe { std::str::from_utf8_unchecked(&outbuf) };
            result_ptr = CString::new(text).unwrap().into_raw();
        }
        else if cmd.eq_ignore_ascii_case("match") {
            let s = match space.lock() {
                Ok(g) => g,
                Err(_) => {
                    result_ptr = CString::new("ERR: space poisoned").unwrap().into_raw();
                    return;
                }
            };
            let mut parsebuf = [0u8; 4096];
            let (qexpr, _used) = match parse_sexpr(&s, inp.as_bytes(), &mut parsebuf) {
                Ok(ok) => ok,
                Err(_) => {
                    result_ptr = CString::new("ERR: parse failed").unwrap().into_raw();
                    return;
                }
            };
            outbuf.clear();
            //Now dump the query results into the outbut buffer:
            s.dump_sexpr(qexpr, qexpr, &mut *outbuf);
            let text = unsafe { std::str::from_utf8_unchecked(&outbuf) };
            result_ptr = CString::new(text).unwrap().into_raw();
        }
    });
    result_ptr
}

//Free allocated C string:
#[no_mangle]
pub extern "C" fn rust_string_free(ptr: *mut c_char) {
    if !ptr.is_null() {
        unsafe { drop(CString::from_raw(ptr)); }
    }
}
