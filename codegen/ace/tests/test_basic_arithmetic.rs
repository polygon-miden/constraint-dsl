mod utils;
use utils::codegen;

static MULTIPLE_ROWS_AIR: &str = "
def MultipleRows

trace_columns {
    main: [a, b],
}

public_inputs {
    stack_inputs: [16],
    stack_outputs: [16],
}

boundary_constraints {
    enf a.first = stack_inputs[0];
    enf a.last = stack_outputs[1];
}

integrity_constraints {
    enf a' = a + 1;
    enf b' = b * 2;
}";

#[test]
fn test_ace() {
    let code = codegen(MULTIPLE_ROWS_AIR);
    std::fs::write("ace.dot", &code).expect("Unable to write file");
}
