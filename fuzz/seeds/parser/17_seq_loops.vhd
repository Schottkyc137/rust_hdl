architecture a of e is
begin
  process is
  begin
    l1 : for i in 0 to 7 loop
      next l1 when i = 3;
      exit l1 when i = 5;
    end loop l1;

    l2 : while cond loop
      exit;
    end loop;

    l3 : loop
      next;
    end loop l3;
  end process;
end architecture;
