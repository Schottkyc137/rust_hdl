architecture a of e is
  constant c1 : t := lib.pkg.obj;
  constant c2 : t := arr(0)(1 to 2)(3 downto 0);
  constant c3 : t := rec.field.sub;
  constant c4 : t := ptr.all;
  constant c5 : t := s'range;
  constant c6 : t := s'reverse_range;
  constant c7 : t := s'left(1);
  constant c8 : t := t'(expr);
  constant c9 : t := t'subtype;
  constant c10 : t := f'["+"];
  constant c11 : t := fn [integer return integer]'attr;
  constant c12 : t := \extended name\;
  constant c13 : t := <<signal .top.a.b : bit>>;
  constant c14 : t := <<variable ^.^.v : integer>>;
  constant c15 : t := <<constant @lib.k : integer>>;
  constant c16 : t := f(a, b => c, others => d);
begin
end architecture;
