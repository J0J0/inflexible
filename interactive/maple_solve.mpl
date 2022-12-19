hasname := proc(x)
  local s:

  s := eval(subs(RootOf=(()->()), x)):
  return hastype([s], 'name');
    # [s] instead of s because s could be NULL
end proc:

solve_constr := proc(constr, id::name)
  local sol_sets, sols, val:
  
  sol_sets := solve(constr):
  
  for sols in sol_sets do
    val := eval(id, sols):

    if val = id then
      printf("*\n"):
    elif type(val, 'rational') then
      printf("Q %d/%d\n", op(1,val), `if`(type(val, 'integer'), 1, op(2,val))):
    elif hasname(val) then
      printf("=\n"):
    else
      printf("C\n"):
    end if:
  end do:

  printf("\n\n\n"):
end proc:
