architecture a of e is
begin
  a1 : x <= y;
  a2 : x <= guarded y after 1 ns;
  a3 : postponed x <= y;
  a4 : x <= y when c = '1' else z when d = '1' else w;
  a5 : with sel select x <= y when 0, z when 1 | 2, w when others;
  a6 : with sel select? x <= y when others;
  a7 : x <= unaffected when c else y;
  a8 : prc(a, b);
  a9 : postponed prc;
  a10 : assert c report "m" severity error;
end architecture;
