configuration cfg of e is
  use work.pkg.all;
  group gt is (signal, label);
  group grp : gt (a, b);
  attribute attr of foo : signal is 1;
  for rtl
    for others : comp use open;
    end for;
    for all : comp2 use configuration work.c2;
    end for;
    for i1 : comp3 use entity work.x(rtl) port map (p => q);
      use vunit v1;
      for blk
      end for;
    end for;
    for i2 : comp4
      use vunit work.v2;
    end for;
  end for;
end configuration;
