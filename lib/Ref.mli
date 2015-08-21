module type S = sig
  type 'a ref
  type ('a,'b) reagent
  val read : 'a ref -> (unit, 'a) reagent
  val cas  : 'a ref -> 'a -> 'a -> (unit, unit) reagent
  val upd  : 'a ref -> ('a * 'b -> ('a *'c) option) -> ('b,'c) reagent
end

module Make(Sched: Scheduler.S)
  : S with type ('a,'b) reagent = ('a,'b) Reagent.Make(Sched).t
