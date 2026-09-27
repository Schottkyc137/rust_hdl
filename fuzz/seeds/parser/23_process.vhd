architecture a of e is
begin
  p1 : process (all) is
  begin
  end process p1;

  p2 : postponed process (a, b)
  begin
  end postponed process p2;

  p3 : process
  begin
  end process;
end architecture;
