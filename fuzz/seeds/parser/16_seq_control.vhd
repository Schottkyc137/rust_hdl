architecture a of e is
begin
  process (clk) is
  begin
    if rst = '1' then
      q <= '0';
    elsif rising_edge(clk) then
      q <= d;
    else
      null;
    end if;

    case sel is
      when 0 =>
        null;
      when 1 | 2 =>
        null;
      when 3 to 5 =>
        null;
      when others =>
        null;
    end case;
  end process;
end architecture;
