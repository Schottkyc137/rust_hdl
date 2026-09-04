architecture a of e is
begin
  b : block (guard_expr) is
    generic (g : integer);
    generic map (g => 1);
    port (p : in bit);
    port map (p => q);
    signal s : bit;
  begin
    s <= '1';
  end block b;
end architecture;
