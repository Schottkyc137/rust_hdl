architecture a of e is
  component comp is
  end component;
  for i1 : comp use entity work.x(rtl) generic map (g => 1) port map (p => q);
  for all : comp use configuration work.cfg;
  for others : comp use open;
  for i2, i3 : comp use entity work.y;
  end for;
  for i4 : comp use entity work.z(rtl);
    use vunit v1;
    use vunit work.v2;
  end for;
begin
end architecture;
