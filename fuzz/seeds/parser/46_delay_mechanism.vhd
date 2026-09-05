architecture a of e is
begin
  c1 : s <= transport a after 1 ns;
  c2 : s <= inertial b;
  c3 : s <= reject 2 ns inertial c after 3 ns;
  process
  begin
    s <= transport a;
    s <= reject 1 ns inertial b, c after 2 ns;
    with sel select s <= transport a when others;
  end process;
end architecture;
