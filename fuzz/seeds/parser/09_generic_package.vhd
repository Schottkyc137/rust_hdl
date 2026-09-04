package gp is
  generic (
    type t;
    n : natural := 4;
    function f (x : t) return t;
    package inner is new work.q generic map (<>)
  );
end package gp;
