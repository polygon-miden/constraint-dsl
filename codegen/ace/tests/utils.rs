use miden_diagnostics::{
    term::termcolor::ColorChoice, CodeMap, DefaultEmitter, DiagnosticsHandler,
};
use std::sync::Arc;

pub fn codegen(source: &str) -> String {
    use air_ir::CodeGenerator;
    use air_pass::Pass;

    let codemap = Arc::new(CodeMap::new());
    let emitter = Arc::new(DefaultEmitter::new(ColorChoice::Auto));
    let diagnostics = DiagnosticsHandler::new(Default::default(), codemap.clone(), emitter);

    let air = air_parser::parse(&diagnostics, codemap, source)
        .map_err(air_ir::CompileError::Parse)
        .and_then(|ast| {
            let mut pipeline = air_parser::transforms::ConstantPropagation::new(&diagnostics)
                .chain(air_parser::transforms::Inlining::new(&diagnostics))
                .chain(air_ir::passes::AstToAir::new(&diagnostics));
            pipeline.run(ast)
        })
        .expect("lowering failed");

    let codegen = air_codegen_ace::CodeGenerator::default();
    codegen.generate(&air).expect("codegen failed")
}
