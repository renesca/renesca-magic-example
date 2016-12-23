object ExampleSchemaTraits {
  import renesca.{graph=>raw};
  import renesca.QueryHandler;
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(Animal.labels, AnimalMatches), scala.Tuple2(Fish.labels, Fish), scala.Tuple2(Dog.labels, Dog));
  trait RootNodeTraitFactory[+NODE <: Node] {
    def factory(node: raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  def setupDbConstraints(queryHandler: QueryHandler) = ();
  trait AnimalMatchesFactory[+NODE <: Animal] extends NodeFactory[NODE] {
    def matchesAnimal(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
  };
  trait AnimalFactory[+NODE <: Animal] extends NodeFactory[NODE] with AnimalMatchesFactory[NODE] {
    def createAnimal(name: String): NODE;
    def mergeAnimal(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
  };
  object Animal extends RootNodeTraitFactory[Animal] with AnimalMatchesFactory[Animal] {
    val label = AnimalMatches.label;
    val labels = AnimalMatches.labels;
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): AnimalMatches = AnimalMatches.matches(name, matches);
    def matchesAnimal(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): AnimalMatches = this.matches(name, matches)
  };
  trait Animal extends Node {
    def eatsFishs: Seq[Fish];
    def eatsDogs: Seq[Dog];
    def drinksFishs: Seq[Fish];
    def drinksDogs: Seq[Dog];
    def eats: Seq[Animal];
    def drinks: Seq[Animal];
    def rev_eatsFishs: Seq[Fish];
    def rev_eatsDogs: Seq[Dog];
    def rev_drinksFishs: Seq[Fish];
    def rev_drinksDogs: Seq[Dog];
    def rev_eats: Seq[Animal];
    def rev_drinks: Seq[Animal];
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue]
  };
  object AnimalMatches extends AnimalMatchesFactory[AnimalMatches] {
    val label = raw.Label("ANIMAL");
    val labels = Set(raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new AnimalMatches(node);
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): AnimalMatches = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      wrapped
    };
    def matchesAnimal(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): AnimalMatches = this.matches(name, matches)
  };
  object Fish extends AnimalFactory[Fish] {
    val label = raw.Label("FISH");
    val labels = Set(raw.Label("FISH"), raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Fish(node);
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Fish = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      wrapped
    };
    def matchesAnimal(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Fish = this.matches(name, matches);
    def create(name: String): Fish = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def merge(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Fish = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def createAnimal(name: String): Fish = this.create(name);
    def mergeAnimal(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Fish = this.merge(name, merge, onMatch)
  };
  object Dog extends AnimalFactory[Dog] {
    val label = raw.Label("DOG");
    val labels = Set(raw.Label("DOG"), raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Dog(node);
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Dog = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      wrapped
    };
    def matchesAnimal(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Dog = this.matches(name, matches);
    def create(name: String): Dog = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def merge(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Dog = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def createAnimal(name: String): Dog = this.create(name);
    def mergeAnimal(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Dog = this.merge(name, merge, onMatch)
  };
  case class AnimalMatches(rawItem: raw.Node) extends Animal {
    override val label = raw.Label("ANIMAL");
    override val labels = Set(raw.Label("ANIMAL"));
    def eatsFishs: Seq[Fish] = successorsAs(Fish, Eats);
    def eatsDogs: Seq[Dog] = successorsAs(Dog, Eats);
    def drinksFishs: Seq[Fish] = successorsAs(Fish, Drinks);
    def drinksDogs: Seq[Dog] = successorsAs(Dog, Drinks);
    def eats: Seq[Animal] = Seq.empty.++(eatsFishs).++(eatsDogs);
    def drinks: Seq[Animal] = Seq.empty.++(drinksFishs).++(drinksDogs);
    def rev_eatsFishs: Seq[Fish] = predecessorsAs(Fish, Eats);
    def rev_eatsDogs: Seq[Dog] = predecessorsAs(Dog, Eats);
    def rev_drinksFishs: Seq[Fish] = predecessorsAs(Fish, Drinks);
    def rev_drinksDogs: Seq[Dog] = predecessorsAs(Dog, Drinks);
    def rev_eats: Seq[Animal] = Seq.empty.++(rev_eatsFishs).++(rev_eatsDogs);
    def rev_drinks: Seq[Animal] = Seq.empty.++(rev_drinksFishs).++(rev_drinksDogs)
  };
  case class Fish(rawItem: raw.Node) extends Animal {
    override val label = raw.Label("FISH");
    override val labels = Set(raw.Label("FISH"), raw.Label("ANIMAL"));
    def eatsFishs: Seq[Fish] = successorsAs(Fish, Eats);
    def eatsDogs: Seq[Dog] = successorsAs(Dog, Eats);
    def drinksFishs: Seq[Fish] = successorsAs(Fish, Drinks);
    def drinksDogs: Seq[Dog] = successorsAs(Dog, Drinks);
    def eats: Seq[Animal] = Seq.empty.++(eatsFishs).++(eatsDogs);
    def drinks: Seq[Animal] = Seq.empty.++(drinksFishs).++(drinksDogs);
    def rev_eatsFishs: Seq[Fish] = predecessorsAs(Fish, Eats);
    def rev_eatsDogs: Seq[Dog] = predecessorsAs(Dog, Eats);
    def rev_drinksFishs: Seq[Fish] = predecessorsAs(Fish, Drinks);
    def rev_drinksDogs: Seq[Dog] = predecessorsAs(Dog, Drinks);
    def rev_eats: Seq[Animal] = Seq.empty.++(rev_eatsFishs).++(rev_eatsDogs);
    def rev_drinks: Seq[Animal] = Seq.empty.++(rev_drinksFishs).++(rev_drinksDogs)
  };
  case class Dog(rawItem: raw.Node) extends Animal {
    override val label = raw.Label("DOG");
    override val labels = Set(raw.Label("DOG"), raw.Label("ANIMAL"));
    def eatsFishs: Seq[Fish] = successorsAs(Fish, Eats);
    def eatsDogs: Seq[Dog] = successorsAs(Dog, Eats);
    def drinksFishs: Seq[Fish] = successorsAs(Fish, Drinks);
    def drinksDogs: Seq[Dog] = successorsAs(Dog, Drinks);
    def eats: Seq[Animal] = Seq.empty.++(eatsFishs).++(eatsDogs);
    def drinks: Seq[Animal] = Seq.empty.++(drinksFishs).++(drinksDogs);
    def rev_eatsFishs: Seq[Fish] = predecessorsAs(Fish, Eats);
    def rev_eatsDogs: Seq[Dog] = predecessorsAs(Dog, Eats);
    def rev_drinksFishs: Seq[Fish] = predecessorsAs(Fish, Drinks);
    def rev_drinksDogs: Seq[Dog] = predecessorsAs(Dog, Drinks);
    def rev_eats: Seq[Animal] = Seq.empty.++(rev_eatsFishs).++(rev_eatsDogs);
    def rev_drinks: Seq[Animal] = Seq.empty.++(rev_drinksFishs).++(rev_drinksDogs)
  };
  trait ConsumesMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node] extends AbstractRelationFactory[START, RELATION, END] {
    def matchesConsumes(startNode: START, endNode: END, funny: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): RELATION
  };
  trait ConsumesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node] extends AbstractRelationFactory[START, RELATION, END] with ConsumesMatchesFactory[START, RELATION, END] {
    def createConsumes(startNode: START, endNode: END, funny: Boolean): RELATION;
    def mergeConsumes(startNode: START, endNode: END, funny: Boolean, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
  };
  trait Consumes[+START <: Node, +END <: Node] extends AbstractRelation[START, END] {
    def funny: Boolean = rawItem.properties("funny").asInstanceOf[BooleanPropertyValue]
  };
  object Eats extends RelationFactory[Animal, Eats, Animal] with ConsumesFactory[Animal, Eats, Animal] {
    val relationType = raw.RelationType("EATS");
    def wrap(relation: raw.Relation) = Eats(Animal.wrap(relation.startNode), relation, Animal.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Animal, funny: Boolean): Eats = {
      val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
      wrapped.rawItem.properties.update("funny", funny);
      wrapped
    };
    def merge(startNode: Animal, endNode: Animal, funny: Boolean, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Eats = {
      val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("funny", funny);
      wrapped
    };
    def matches(startNode: Animal, endNode: Animal, funny: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): Eats = {
      val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
      if (funny.isDefined)
        wrapped.rawItem.properties.update("funny", funny.get)
      else
        ();
      wrapped
    };
    def createConsumes(startNode: Animal, endNode: Animal, funny: Boolean): Eats = this.create(startNode, endNode, funny);
    def mergeConsumes(startNode: Animal, endNode: Animal, funny: Boolean, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Eats = this.merge(startNode, endNode, funny, merge, onMatch);
    def matchesConsumes(startNode: Animal, endNode: Animal, funny: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): Eats = this.matches(startNode, endNode, funny, matches)
  };
  object Drinks extends RelationFactory[Animal, Drinks, Animal] with ConsumesFactory[Animal, Drinks, Animal] {
    val relationType = raw.RelationType("DRINKS");
    def wrap(relation: raw.Relation) = Drinks(Animal.wrap(relation.startNode), relation, Animal.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Animal, funny: Boolean): Drinks = {
      val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
      wrapped.rawItem.properties.update("funny", funny);
      wrapped
    };
    def merge(startNode: Animal, endNode: Animal, funny: Boolean, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Drinks = {
      val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("funny", funny);
      wrapped
    };
    def matches(startNode: Animal, endNode: Animal, funny: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): Drinks = {
      val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
      if (funny.isDefined)
        wrapped.rawItem.properties.update("funny", funny.get)
      else
        ();
      wrapped
    };
    def createConsumes(startNode: Animal, endNode: Animal, funny: Boolean): Drinks = this.create(startNode, endNode, funny);
    def mergeConsumes(startNode: Animal, endNode: Animal, funny: Boolean, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Drinks = this.merge(startNode, endNode, funny, merge, onMatch);
    def matchesConsumes(startNode: Animal, endNode: Animal, funny: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): Drinks = this.matches(startNode, endNode, funny, matches)
  };
  case class Eats(startNode: Animal, rawItem: raw.Relation, endNode: Animal) extends Relation[Animal, Animal] with Consumes[Animal, Animal];
  case class Drinks(startNode: Animal, rawItem: raw.Relation, endNode: Animal) extends Relation[Animal, Animal] with Consumes[Animal, Animal];
  object WholeExampleSchemaTraits {
    def empty = new WholeExampleSchemaTraits(raw.Graph.empty);
    def remove(items: Item*) = {
      val wrapper = empty;
      wrapper.remove(((items): _*));
      wrapper
    };
    def apply(items: Item*) = {
      val wrapper = empty;
      wrapper.add(((items): _*));
      wrapper
    }
  };
  object Zoo {
    def empty = new Zoo(raw.Graph.empty);
    def remove(items: Item*) = {
      val wrapper = empty;
      wrapper.remove(((items): _*));
      wrapper
    };
    def apply(items: Item*) = {
      val wrapper = empty;
      wrapper.add(((items): _*));
      wrapper
    }
  };
  case class WholeExampleSchemaTraits(graph: raw.Graph) extends Graph {
    def fishs: Seq[Fish] = nodesAs(Fish);
    def dogs: Seq[Dog] = nodesAs(Dog);
    def eats: Seq[Eats] = relationsAs(Eats);
    def drinks: Seq[Drinks] = relationsAs(Drinks);
    def animals: Seq[Animal] = Seq.empty.++(fishs).++(dogs);
    def animalRelations: (Seq[_$37] forSome { 
      type _$37 <: Relation[Animal, Animal]
    }) = Seq.empty.++(eats).++(drinks);
    def animalAbstractRelations: (Seq[_$38] forSome { 
      type _$38 <: AbstractRelation[Animal, Animal]
    }) = Seq.empty.++(eats).++(drinks);
    def animalHyperRelations: Seq[(HyperRelation[Animal, _$39, _$46, _$45, Animal] forSome { 
      type _$39 <: (Relation[Animal, _$43] forSome { 
        type _$43
      });
      type _$46 <: (HyperRelation[Animal, _$44, _$42, _$40, Animal] forSome { 
        type _$44;
        type _$42;
        type _$40
      });
      type _$45 <: (Relation[_$41, Animal] forSome { 
        type _$41
      })
    })] = Seq.empty;
    def nodes: Seq[Node] = Seq.empty.++(fishs).++(dogs);
    def relations: (Seq[_$48] forSome { 
      type _$48 <: (Relation[_$54, _$57] forSome { 
        type _$54;
        type _$57
      })
    }) = Seq.empty.++(eats).++(drinks);
    def abstractRelations: (Seq[_$56] forSome { 
      type _$56 <: (AbstractRelation[_$53, _$51] forSome { 
        type _$53;
        type _$51
      })
    }) = Seq.empty.++(eats).++(drinks);
    def hyperRelations: (Seq[_$55] forSome { 
      type _$55 <: (HyperRelation[_$52, _$50, _$49, _$47, _$58] forSome { 
        type _$52;
        type _$50;
        type _$49;
        type _$47;
        type _$58
      })
    }) = Seq.empty
  };
  case class Zoo(graph: raw.Graph) extends Graph {
    def fishs: Seq[Fish] = nodesAs(Fish);
    def dogs: Seq[Dog] = nodesAs(Dog);
    def eats: Seq[Eats] = relationsAs(Eats);
    def drinks: Seq[Drinks] = relationsAs(Drinks);
    def animals: Seq[Animal] = Seq.empty.++(fishs).++(dogs);
    def animalRelations: (Seq[_$59] forSome { 
      type _$59 <: Relation[Animal, Animal]
    }) = Seq.empty.++(eats).++(drinks);
    def animalAbstractRelations: (Seq[_$60] forSome { 
      type _$60 <: AbstractRelation[Animal, Animal]
    }) = Seq.empty.++(eats).++(drinks);
    def animalHyperRelations: Seq[(HyperRelation[Animal, _$61, _$68, _$67, Animal] forSome { 
      type _$61 <: (Relation[Animal, _$65] forSome { 
        type _$65
      });
      type _$68 <: (HyperRelation[Animal, _$66, _$64, _$62, Animal] forSome { 
        type _$66;
        type _$64;
        type _$62
      });
      type _$67 <: (Relation[_$63, Animal] forSome { 
        type _$63
      })
    })] = Seq.empty;
    def nodes: Seq[Node] = Seq.empty.++(fishs).++(dogs);
    def relations: (Seq[_$70] forSome { 
      type _$70 <: (Relation[_$76, _$79] forSome { 
        type _$76;
        type _$79
      })
    }) = Seq.empty.++(eats).++(drinks);
    def abstractRelations: (Seq[_$78] forSome { 
      type _$78 <: (AbstractRelation[_$75, _$73] forSome { 
        type _$75;
        type _$73
      })
    }) = Seq.empty.++(eats).++(drinks);
    def hyperRelations: (Seq[_$77] forSome { 
      type _$77 <: (HyperRelation[_$74, _$72, _$71, _$69, _$80] forSome { 
        type _$74;
        type _$72;
        type _$71;
        type _$69;
        type _$80
      })
    }) = Seq.empty
  }
}
