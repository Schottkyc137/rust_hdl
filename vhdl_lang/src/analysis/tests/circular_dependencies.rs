// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::data::error_codes::ErrorCode;

#[test]
fn context() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "

context ctx1 is
  library libname;
  context libname.ctx2;
end context;

context ctx2 is
  library libname;
  context libname.ctx1;
end context;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("ctx1", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s("ctx2", 1),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn use_package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.const;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("pkg1", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s("pkg2", 1),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn use_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

use work.pkg2.const;
package pkg1 is new work.gpkg generic map(c => false);

use work.pkg1.const;
package pkg2 is new work.gpkg generic map(c => true);
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("pkg1", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s1("pkg2"),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn package_instance_in_declarative_region() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.const;
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

package pkg2 is
   package ipkg is new work.gpkg generic map(c => false);
   constant const : boolean := false;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s1("pkg2"),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s("gpkg", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn package_instance_in_interface() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg3.const;
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

package pkg2 is
   generic (
      package ipkg is new work.gpkg generic map(c => true)
   );
   constant const : boolean := false;
end package;

package pkg3 is new work.pkg2;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s1("pkg3"),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s("gpkg", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s("pkg2", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn use_package_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.all;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("pkg1", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s1("pkg2"),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn use_package_instance_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (g : boolean);
end package;

use work.pkg2.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.all;

package pkg2 is new work.gpkg generic map (g => true);
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("pkg1", 2),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
            Diagnostic::new(
                code.s1("pkg2"),
                "Found circular dependency",
                ErrorCode::CircularDependency,
            ),
        ],
    );
}

#[test]
fn use_library_all() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
use work.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    check_no_diagnostics(&builder.analyze());
}

#[test]
fn circular_dependency_own_library_all() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "lib",
        "
library lib;
use lib.all;

entity a is
end entity;

architecture a_rtl of a is
begin
end;",
    );
    // This is allowed because 'lib' refers to it's own library
    // (similar to `library work` statement)
    check_no_diagnostics(&builder.analyze());
}

#[test]
fn circular_dependency_own_library() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "lib",
        "
package foo is
    constant bar : bit := '0';
end foo;

library lib;
use lib.foo.bar;

entity a is
end entity;
",
    );
    check_no_diagnostics(&builder.analyze());
}
