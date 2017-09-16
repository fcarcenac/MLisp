let input_channel = ref stdin
let output_channel = ref stdout

let init_env = ref (LspUtils.init 769 769 0 "MAIN")

let history = ".mlisp.history"

let command = ref ""

let lib_path = "./lib"
