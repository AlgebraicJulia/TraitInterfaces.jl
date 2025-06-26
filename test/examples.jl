module TestExamples 

using TraitInterfaces, Test

# https://doc.rust-lang.org/rust-by-example/trait.html
#######################################################

@interface Animal begin
  @import String::TYPE
  name()::String
  noise()::String
end 

struct Sheep 
  naked::Bool 
  name::String 
end

joe = Sheep(true, "Joe")

@instance Animal [model::Sheep] begin 
  name()::String = model.name
  noise()::String = model.naked ? "baaaaah?" : "baaaaah!"
end

@test name[joe]() == "Joe"
@test noise[joe]() == "baaaaah?"

# Another way of doing this 

@interface Animal′ begin
  Species::TYPE 
  @import String::TYPE
  name(s::Species)::String
  noise(s::Species)::String
end 

struct SheepImplsAnimalTrait end
trait = SheepImplsAnimalTrait()

@instance Animal′{Species=Sheep} [model::SheepImplsAnimalTrait] begin 
  name(s::Sheep)::String = s.name
  noise(s::Sheep)::String = s.naked ? "baaaaah?" : "baaaaah!"
end

@test name[trait](joe) == "Joe"
@test noise[trait](joe) == "baaaaah?"


# Disambiguating when the same type implements two interfaces which have 
# (conflicting) methods with the same name.
# https://doc.rust-lang.org/rust-by-example/trait/disambiguating.html
#####################################################################

module NamedModule
  export Named
  using TraitInterfaces: @interface
  @interface Named begin 
    @import String::TYPE
    get()::String 
  end
end
using .NamedModule

@instance Named [model::Sheep] begin 
  get()::String = model.name
end


module NoisedModule 
  export Noised
  using TraitInterfaces: @interface
  @interface Noised begin 
    @import String::TYPE
    get()::String 
  end
end

using .NoisedModule

@instance Noised [model::Sheep] begin 
  get()::String = Animal.noise[model]()
end

@test Named.get[joe]() == "Joe"
@test Noised.get[joe]() == "baaaaah?"

end # module
