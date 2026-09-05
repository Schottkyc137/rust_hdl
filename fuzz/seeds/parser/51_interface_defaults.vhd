entity e is
  generic (
    procedure p1 is <>;
    procedure p2 (x : bit) is work.pkg.q;
    function f1 return bit is <>;
    impure function f2 (a : bit) return bit is work.pkg.g;
    package pk1 is new work.gp generic map (<>);
    package pk2 is new work.gp generic map (default);
    package pk3 is new work.gp generic map (g => 1)
  );
end entity;
