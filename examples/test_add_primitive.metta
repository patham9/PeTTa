!(add-atom &space "Hello")
!(add-atom &space 42)
!(add-atom &space (A B))

; This will list all atoms currently in the space
!(match &space $x $x)
