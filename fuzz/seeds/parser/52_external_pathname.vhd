architecture a of e is
begin
  process
  begin
    x <= << signal .tb.gen(0).s : bit >>;
    y <= << variable @work.pkg.v : integer >>;
    z <= << constant ^.^.c : bit >>;
    w <= << signal g(1)(2).s : bit >>;
  end process;
end architecture;
