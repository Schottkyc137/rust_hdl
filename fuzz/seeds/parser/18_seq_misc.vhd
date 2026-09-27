architecture a of e is
begin
  process is
    variable v : integer;
  begin
    wait;
    wait on a, b until c = '1' for 10 ns;
    wait for 1 ps;
    assert x = y;
    assert x = y report "msg";
    assert x = y report "msg" severity note;
    report "hello" severity warning;
    v := 1 + 2;
    s <= '1';
    s <= '0' after 1 ns, '1' after 2 ns;
    s <= transport '1' after 1 ns;
    s <= reject 1 ns inertial '1';
    s <= force in '1';
    s <= release out;
    s <= null;
    prc(1, 2);
    prc(a => 1, b => open);
    return;
    l : null;
  end process;
end architecture;
