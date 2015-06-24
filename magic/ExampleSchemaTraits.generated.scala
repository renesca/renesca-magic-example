object ExampleSchemaTraits {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$15] forSome { 
    type _$15 <: Node
  })](scala.Tuple2("FISH", Fish), scala.Tuple2("DOG", Dog));
  trait RootNodeTraitFactory[NODE <: Node] {
    val nodeLabels: Set[raw.Label] = Set("FISH", "DOG");
    def nodeLabel(node: raw.Node): raw.Label = nodeLabels.intersect(node.labels).head;
    def factory(node: raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  trait AnimalFactory[NODE <: Animal] extends NodeFactory[NODE] {
    def createAnimal(name: String): NODE
  };
  object Animal extends RootNodeTraitFactory[Animal] {
    val label = raw.Label("ANIMAL");
    val labels = Set(raw.Label("ANIMAL"))
  };
  trait Animal extends Node {
    def name: String = node.properties("name").asInstanceOf[StringPropertyValue]
  };
  object Fish extends AnimalFactory[Fish] {
    val label = raw.Label("FISH");
    val labels = Set(raw.Label("FISH"), raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Fish(node);
    def create(name: String): Fish = {
      val node = wrap(raw.Node.create(labels));
      node.node.properties.update("name", name);
      node
    };
    def createAnimal(name: String): Fish = create(name)
  };
  object Dog extends AnimalFactory[Dog] {
    val label = raw.Label("DOG");
    val labels = Set(raw.Label("DOG"), raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Dog(node);
    def create(name: String): Dog = {
      val node = wrap(raw.Node.create(labels));
      node.node.properties.update("name", name);
      node
    };
    def createAnimal(name: String): Dog = create(name)
  };
  case class Fish(node: raw.Node) extends Animal {
    override val label = raw.Label("FISH");
    override val labels = Set(raw.Label("FISH"), raw.Label("ANIMAL"));
    def eatsFishs: Set[Fish] = successorsAs(Fish, Eats);
    def eatsDogs: Set[Dog] = successorsAs(Dog, Eats);
    def drinksFishs: Set[Fish] = successorsAs(Fish, Drinks);
    def drinksDogs: Set[Dog] = successorsAs(Dog, Drinks);
    def eats: Set[Animal] = Set.empty.++(eatsFishs).++(eatsDogs);
    def drinks: Set[Animal] = Set.empty.++(drinksFishs).++(drinksDogs);
    def rev_eatsFishs: Set[Fish] = predecessorsAs(Fish, Eats);
    def rev_eatsDogs: Set[Dog] = predecessorsAs(Dog, Eats);
    def rev_drinksFishs: Set[Fish] = predecessorsAs(Fish, Drinks);
    def rev_drinksDogs: Set[Dog] = predecessorsAs(Dog, Drinks);
    def rev_eats: Set[Animal] = Set.empty.++(rev_eatsFishs).++(rev_eatsDogs);
    def rev_drinks: Set[Animal] = Set.empty.++(rev_drinksFishs).++(rev_drinksDogs)
  };
  case class Dog(node: raw.Node) extends Animal {
    override val label = raw.Label("DOG");
    override val labels = Set(raw.Label("DOG"), raw.Label("ANIMAL"));
    def eatsFishs: Set[Fish] = successorsAs(Fish, Eats);
    def eatsDogs: Set[Dog] = successorsAs(Dog, Eats);
    def drinksFishs: Set[Fish] = successorsAs(Fish, Drinks);
    def drinksDogs: Set[Dog] = successorsAs(Dog, Drinks);
    def eats: Set[Animal] = Set.empty.++(eatsFishs).++(eatsDogs);
    def drinks: Set[Animal] = Set.empty.++(drinksFishs).++(drinksDogs);
    def rev_eatsFishs: Set[Fish] = predecessorsAs(Fish, Eats);
    def rev_eatsDogs: Set[Dog] = predecessorsAs(Dog, Eats);
    def rev_drinksFishs: Set[Fish] = predecessorsAs(Fish, Drinks);
    def rev_drinksDogs: Set[Dog] = predecessorsAs(Dog, Drinks);
    def rev_eats: Set[Animal] = Set.empty.++(rev_eatsFishs).++(rev_eatsDogs);
    def rev_drinks: Set[Animal] = Set.empty.++(rev_drinksFishs).++(rev_drinksDogs)
  };
  trait ConsumesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node] extends AbstractRelationFactory[START, RELATION, END] {
    def createConsumes(startNode: START, endNode: END): RELATION
  };
  trait Consumes[+START <: Node, +END <: Node] extends AbstractRelation[START, END];
  object Eats extends RelationFactory[Animal, Eats, Animal] with ConsumesFactory[Animal, Eats, Animal] {
    val relationType = raw.RelationType("EATS");
    def wrap(relation: raw.Relation) = Eats(Animal.wrap(relation.startNode), relation, Animal.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Animal): Eats = {
      val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      relation
    };
    def createConsumes(startNode: Animal, endNode: Animal): Eats = create(startNode, endNode)
  };
  object Drinks extends RelationFactory[Animal, Drinks, Animal] with ConsumesFactory[Animal, Drinks, Animal] {
    val relationType = raw.RelationType("DRINKS");
    def wrap(relation: raw.Relation) = Drinks(Animal.wrap(relation.startNode), relation, Animal.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Animal): Drinks = {
      val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      relation
    };
    def createConsumes(startNode: Animal, endNode: Animal): Drinks = create(startNode, endNode)
  };
  case class Eats(startNode: Animal, relation: raw.Relation, endNode: Animal) extends Relation[Animal, Animal] with Consumes[Animal, Animal];
  case class Drinks(startNode: Animal, relation: raw.Relation, endNode: Animal) extends Relation[Animal, Animal] with Consumes[Animal, Animal];
  object Zoo {
    def empty = new Zoo(raw.Graph.empty)
  };
  case class Zoo(graph: raw.Graph) extends Graph {
    def fishs: Set[Fish] = nodesAs(Fish);
    def dogs: Set[Dog] = nodesAs(Dog);
    def eats: Set[Eats] = relationsAs(Eats);
    def drinks: Set[Drinks] = relationsAs(Drinks);
    def animals: Set[Animal] = Set.empty.++(fishs).++(dogs);
    def animalRelations: (Set[_$16] forSome { 
      type _$16 <: Relation[Animal, Animal]
    }) = Set.empty.++(eats).++(drinks);
    def animalAbstractRelations: (Set[_$17] forSome { 
      type _$17 <: AbstractRelation[Animal, Animal]
    }) = Set.empty.++(eats).++(drinks);
    def animalHyperRelations: Set[(HyperRelation[Animal, _$18, _$25, _$24, Animal] forSome { 
      type _$18 <: (Relation[Animal, _$22] forSome { 
        type _$22
      });
      type _$25 <: (HyperRelation[Animal, _$23, _$21, _$19, Animal] forSome { 
        type _$23;
        type _$21;
        type _$19
      });
      type _$24 <: (Relation[_$20, Animal] forSome { 
        type _$20
      })
    })] = Set.empty;
    def nodes: Set[Node] = Set.empty.++(fishs).++(dogs);
    def relations: (Set[_$27] forSome { 
      type _$27 <: (Relation[_$33, _$36] forSome { 
        type _$33;
        type _$36
      })
    }) = Set.empty.++(eats).++(drinks);
    def abstractRelations: (Set[_$35] forSome { 
      type _$35 <: (AbstractRelation[_$32, _$30] forSome { 
        type _$32;
        type _$30
      })
    }) = Set.empty.++(eats).++(drinks);
    def hyperRelations: (Set[_$34] forSome { 
      type _$34 <: (HyperRelation[_$31, _$29, _$28, _$26, _$37] forSome { 
        type _$31;
        type _$29;
        type _$28;
        type _$26;
        type _$37
      })
    }) = Set.empty
  }
}
