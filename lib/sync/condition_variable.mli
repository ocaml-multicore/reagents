module type S = Condition_variable_intf.S

module Make
    (Base : Base.S)
    (Lock : Lock.S with type ('a, 'b) reagent = ('a, 'b) Base.t) :
  S with type ('a, 'b) reagent = ('a, 'b) Base.t and type lock = Lock.t
