architecture a of e is
begin
  c1 : (a, b) <= c;
  c2 : (x => p, y => q) <= r;
  c3 : (others => z) <= w;
  process
  begin
    (a, b) := c;
    (x => p, y => q) <= r after 1 ns;
  end process;
end architecture;
