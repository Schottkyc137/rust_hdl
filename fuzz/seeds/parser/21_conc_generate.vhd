architecture a of e is
begin
  g1 : for i in 0 to 3 generate
    signal s : bit;
  begin
    s <= '1';
  end generate g1;

  g2 : if lbl : c generate
  elsif d generate
  else generate
  end generate g2;

  g3 : case sel generate
    when a : 0 =>
    when others =>
  end generate g3;
end architecture;
