module TestSpecialModels

using Test, TraitInterfaces

@theory ThCat begin
  Ob::TYPE; Hom(dom::Ob,codom::Ob)::TYPE;
  id(a::Ob)::Hom(a,a)
  compose(f::Hom(a,b),g::Hom(b,c))::Hom(a,c) ⊣ [(a,b,c)::Ob]
end

using .ThCat

# Test default model + dispatch model
#####################################
@test_throws MethodError id(2)

d = Dispatch(ThCat, [Int, Vector{Int}])

# False: there exists no id(::String)
@test !implements(d, ThCat, :id, [String])

ThCat.compose(i::Vector{Int},j::Vector{Int}) = i

# Now there is one
@test implements(d, ThCat, :compose, [Vector{Int},Vector{Int}])

# No compose(::Int,::Int) method
@test !implements(Dispatch, ThCat, :id, [Int,Vector{Int}])

ThCat.id(i::Int) = [i]

@test implements(d, ThCat, :id, [Int])
@test implements(d, ThCat, [Int, Vector{Int}])

@test id(1) == [1] == ThCat.id[d](1)
@test compose[d]([1],[2,3]) == [1] == compose(Trait(d), [1],[2,3])

@test implements(d, ThCat, [Int, Vector{Int}])

@test impl_type(d, ThCat.Ob) == Int 
@test impl_type(d, ThCat.Hom) == Vector{Int}

end # module
