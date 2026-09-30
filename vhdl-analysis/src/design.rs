use vhdl_syntax::syntax::{
    ConfigurationDeclarationSyntax, ContextDeclarationSyntax, EntityDeclarationSyntax,
    LibraryUnitSyntax, PackageInstantiationDeclarationPrimaryUnitSyntax,
    PrimaryUnitPackageDeclarationSyntax, PrimaryUnitSyntax, SyntaxToken,
};

use crate::db::{Db, DesignUnit, FileId};

/// Compute all design units that are part of a single file
pub(crate) fn analyze_design(db: &Db, id: FileId) -> Vec<DesignUnit> {
    let ast = db.ast(id);
    let mut units = Vec::new();
    for library_unit in ast.design_units().filter_map(|unit| unit.library_unit()) {
        let unit = match library_unit {
            LibraryUnitSyntax::PrimaryUnit(primary) => match primary {
                PrimaryUnitSyntax::EntityDeclaration(decl) => analyze_entity_decl(&decl),
                PrimaryUnitSyntax::ConfigurationDeclaration(decl) => analyze_config_decl(&decl),
                PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(decl) => {
                    analyze_package_decl(&decl)
                }
                PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(decl) => {
                    analyze_package_instantiation_decl(&decl)
                }
                PrimaryUnitSyntax::ContextDeclaration(decl) => analyze_context_decl(&decl),
            },
            LibraryUnitSyntax::SecondaryUnit(_secondary) => {
                unimplemented!("Requires name analysis")
            }
        };
        if let Some(unit) = unit {
            units.push(unit);
        }
    }
    units
}

fn from_tok_primary(tok: SyntaxToken) -> Option<DesignUnit> {
    Some(DesignUnit::primary(tok.text().to_owned()))
}

fn analyze_entity_decl(decl: &EntityDeclarationSyntax) -> Option<DesignUnit> {
    from_tok_primary(decl.entity_declaration_preamble()?.identifier_token()?)
}

fn analyze_config_decl(decl: &ConfigurationDeclarationSyntax) -> Option<DesignUnit> {
    from_tok_primary(
        decl.configuration_declaration_preamble()?
            .identifier_token()?,
    )
}

fn analyze_package_decl(decl: &PrimaryUnitPackageDeclarationSyntax) -> Option<DesignUnit> {
    from_tok_primary(
        decl.package_declaration()?
            .package_preamble()?
            .identifier_token()?,
    )
}

fn analyze_package_instantiation_decl(
    decl: &PackageInstantiationDeclarationPrimaryUnitSyntax,
) -> Option<DesignUnit> {
    from_tok_primary(
        decl.package_instantiation_declaration()?
            .package_instantiation_preamble()?
            .identifier_token()?,
    )
}

fn analyze_context_decl(decl: &ContextDeclarationSyntax) -> Option<DesignUnit> {
    from_tok_primary(decl.context_declaration_preamble()?.identifier_token()?)
}
