! 成績リストの並べ替え（リスト構造）
program ex9_8

  implicit none
  integer :: tensu
  character (len=12) :: namae
  type unit
    character (len=12) :: name
    integer :: mark
    type(unit), pointer :: next !自分と同じ型の成分をもつ
  end Type
  type (unit), pointer :: ent,new,p
  allocate(ent)
  nullify(ent%next)

   do
    print *, 'Input name (or) [Enter] only to stop:'
    read (*,'(A)') namae
     if (namae==' ') exit
    print *, 'Input her or his mark:'
    read *, tensu

    allocate(new)
    new=unit(namae, tensu, ent%next)
    ent%next => new
   end do

  !出力
  p => ent%next
  do while (associated(p))
    print '(A12, I4)', p%name, p%mark
    p => p%next
  end do

end program ex9_8
