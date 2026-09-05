architecture a of e is
  signal s : bit;
begin
  process
    variable v : integer;
  begin
    v := 1 when c else 2;
    s <= force a when c else b;
    s <= force in a when c else b;
    s <= force out a;
    s <= release out;
    with sel select v := 1 when 0, 2 when others;
    with sel select? v := 1 when others;
    with sel select s <= force in a when 0, b when others;
    with sel select s <= force b when others;
    with sel select s <= a after 1 ns when 0, b when others;
  end process;
end architecture;
