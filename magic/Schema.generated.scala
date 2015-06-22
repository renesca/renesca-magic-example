object Schema {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$1] forSome { 
    type _$1 <: Node
  })](scala.Tuple2("ANIMAL", Animal));
  trait RootNodeTraitFactory[NODE <: Node] {
    val nodeLabels: Set[raw.Label] = Set("ANIMAL");
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
  case class Animal(node: raw.Node) extends Node {
    override val label = raw.Label("ANIMAL");
    override val labels = Set(raw.Label("ANIMAL"));
    def eats: Set[Animal] = successorsAs(Animal, Eats);
    def rev_eats: Set[Animal] = predecessorsAs(Animal, Eats);
    def name: String = node.properties("name").asInstanceOf[StringPropertyValue]
  };
  object Eats extends RelationFactory[Animal, Eats, Animal] with AbstractRelationFactory[Animal, Eats, Animal] {
    val relationType = raw.RelationType("EATS");
    def wrap(relation: raw.Relation) = Eats(Animal.wrap(relation.startNode), relation, Animal.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Animal): Eats = {
      val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      relation
    }
  };
  case class Eats(startNode: Animal, relation: raw.Relation, endNode: Animal) extends Relation[Animal, Animal];
  object Zoo {
    def empty = new Zoo(raw.Graph.empty)
  };
  case class Zoo(graph: raw.Graph) extends Graph {
    def animals: Set[Animal] = nodesAs(Animal);
    def eats: Set[Eats] = relationsAs(Eats);
    def nodes: Set[Node] = Set.empty.++(animals);
    def relations: (Set[_$3] forSome { 
      type _$3 <: (Relation[_$9, _$12] forSome { 
        type _$9;
        type _$12
      })
    }) = Set.empty.++(eats);
    def abstractRelations: (Set[_$11] forSome { 
      type _$11 <: (AbstractRelation[_$8, _$6] forSome { 
        type _$8;
        type _$6
      })
    }) = Set.empty.++(eats);
    def hyperRelations: (Set[_$10] forSome { 
      type _$10 <: (HyperRelation[_$7, _$5, _$4, _$2, _$13] forSome { 
        type _$7;
        type _$5;
        type _$4;
        type _$2;
        type _$13
      })
    }) = Set.empty
  }
}
