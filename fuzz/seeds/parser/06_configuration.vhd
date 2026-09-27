configuration cfg of e is
  for rtl
    for all : comp
      use entity work.e(rtl)
        generic map (g => 1)
        port map (p => q);
    end for;
  end for;
end configuration cfg;
