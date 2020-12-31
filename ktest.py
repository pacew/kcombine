

import sch

val = sch.Sch().readfile('flat/flat.kicad_sch')
print(sch.sexp_to_str(val))
