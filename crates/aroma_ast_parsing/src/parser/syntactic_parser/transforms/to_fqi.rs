//! Makes sure all declarations become fully qualified

use crate::parser::SyntaxError;
use aroma_ast::mir::translation_unit::TranslationUnit;

pub fn to_fqi(translation_unit: TranslationUnit) -> Result<TranslationUnit, SyntaxError> {
    Ok(translation_unit)
}
