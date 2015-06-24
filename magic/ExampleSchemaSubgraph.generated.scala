object ExampleSchemaSubgraph {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$2] forSome { 
    type _$2 <: Node
  })](scala.Tuple2("ANIMAL", Animal), scala.Tuple2("FOOD", Food));
  trait RootNodeTraitFactory[NODE <: Node] {
    val nodeLabels: Set[raw.Label] = Set("ANIMAL", "FOOD");
    def nodeLabel(node: raw.Node): raw.Label = nodeLabels.intersect(node.labels).head;
    def factory(node: raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  object Animal extends NodeFactory[Animal] {
    val label = raw.Label("ANIMAL");
    val labels = Set(raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Animal(node);
    def create(name: String): Animal = {
      val node = wrap(raw.Node.create(labels));
      node.node.properties.update("name", name);
      node
    }
  };
  object Food extends NodeFactory[Food] {
    val label = raw.Label("FOOD");
    val labels = Set(raw.Label("FOOD"));
    def wrap(node: raw.Node) = new Food(node);
    def create(amount: Long, name: String): Food = {
      val node = wrap(raw.Node.create(labels));
      node.node.properties.update("amount", amount);
      node.node.properties.update("name", name);
      node
    }
  };
  case class Animal(node: raw.Node) extends Node {
    override val label = raw.Label("ANIMAL");
    override val labels = Set(raw.Label("ANIMAL"));
    def eats: Set[Food] = successorsAs(Food, Eats);
    def name: String = node.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class Food(node: raw.Node) extends Node {
    override val label = raw.Label("FOOD");
    override val labels = Set(raw.Label("FOOD"));
    def rev_eats: Set[Animal] = predecessorsAs(Animal, Eats);
    def name: String = node.properties("name").asInstanceOf[StringPropertyValue];
    def amount: Long = node.properties("amount").asInstanceOf[LongPropertyValue];
    def `amount_=`(newValue: Long): scala.Unit = node.properties.update("amount", newValue)
  };
  object Eats extends RelationFactory[Animal, Eats, Food] with AbstractRelationFactory[Animal, Eats, Food] {
    val relationType = raw.RelationType("EATS");
    def wrap(relation: raw.Relation) = Eats(Animal.wrap(relation.startNode), relation, Food.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Food): Eats = {
      val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      relation
    }
  };
  case class Eats(startNode: Animal, relation: raw.Relation, endNode: Food) extends Relation[Animal, Food];
  object Zoo {
    def empty = new Zoo(raw.Graph.empty)
  };
  case class Zoo(graph: raw.Graph) extends Graph {
    def animals: Set[Animal] = nodesAs(Animal);
    def foods: Set[Food] = nodesAs(Food);
    def eats: Set[Eats] = relationsAs(Eats);
    def nodes: Set[Node] = Set.empty.++(animals).++(foods);
    def relations: (Set[_$4] forSome { 
      type _$4 <: (Relation[_$10, _$13] forSome { 
        type _$10;
        type _$13
      })
    }) = Set.empty.++(eats);
    def abstractRelations: (Set[_$12] forSome { 
      type _$12 <: (AbstractRelation[_$9, _$7] forSome { 
        type _$9;
        type _$7
      })
    }) = Set.empty.++(eats);
    def hyperRelations: (Set[_$11] forSome { 
      type _$11 <: (HyperRelation[_$8, _$6, _$5, _$3, _$14] forSome { 
        type _$8;
        type _$6;
        type _$5;
        type _$3;
        type _$14
      })
    }) = Set.empty
  }
}