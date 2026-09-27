architecture a of e is
  component comp is
    generic (g : integer := 0);
    port (p : in bit);
  end component comp;
begin
  i1 : comp port map (p => q);
  i2 : component comp generic map (g => 1) port map (q);
  i3 : entity work.e(rtl) port map (open, p => q);
  i4 : configuration work.cfg port map (p => q);
end architecture;
