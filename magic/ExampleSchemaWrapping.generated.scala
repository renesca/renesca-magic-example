object ExampleSchemaWrapping {
  import renesca.{graph=>raw};
  import renesca.QueryHandler;
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(Animal.labels, Animal), scala.Tuple2(Food.labels, Food));
  trait RootNodeTraitFactory[+NODE <: Node] {
    def factory(node: raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  def setupDbConstraints(queryHandler: QueryHandler) = ();
  object Animal extends NodeFactory[Animal] {
    val label = raw.Label("ANIMAL");
    val labels = Set(raw.Label("ANIMAL"));
    def wrap(node: raw.Node) = new Animal(node);
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Animal = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      wrapped
    };
    def create(name: String): Animal = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def merge(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Animal = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("name", name);
      wrapped
    }
  };
  object Food extends NodeFactory[Food] {
    val label = raw.Label("FOOD");
    val labels = Set(raw.Label("FOOD"));
    def wrap(node: raw.Node) = new Food(node);
    def matches(amount: Option[Long] = None, name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Food = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (amount.isDefined)
        wrapped.rawItem.properties.update("amount", amount.get)
      else
        ();
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      wrapped
    };
    def create(amount: Long, name: String): Food = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("amount", amount);
      wrapped.rawItem.properties.update("name", name);
      wrapped
    };
    def merge(amount: Long, name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Food = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("amount", amount);
      wrapped.rawItem.properties.update("name", name);
      wrapped
    }
  };
  case class Animal(rawItem: raw.Node) extends Node {
    override val label = raw.Label("ANIMAL");
    override val labels = Set(raw.Label("ANIMAL"));
    def eats: Seq[Food] = successorsAs(Food, Eats);
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class Food(rawItem: raw.Node) extends Node {
    override val label = raw.Label("FOOD");
    override val labels = Set(raw.Label("FOOD"));
    def rev_eats: Seq[Animal] = predecessorsAs(Animal, Eats);
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue];
    def amount: Long = rawItem.properties("amount").asInstanceOf[LongPropertyValue];
    def `amount_=`(newValue: Long): scala.Unit = rawItem.properties.update("amount", newValue)
  };
  object Eats extends RelationFactory[Animal, Eats, Food] with AbstractRelationFactory[Animal, Eats, Food] {
    val relationType = raw.RelationType("EATS");
    def wrap(relation: raw.Relation) = Eats(Animal.wrap(relation.startNode), relation, Food.wrap(relation.endNode));
    def create(startNode: Animal, endNode: Food): Eats = {
      val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
      wrapped
    };
    def merge(startNode: Animal, endNode: Food, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Eats = {
      val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
      wrapped
    };
    def matches(startNode: Animal, endNode: Food, matches: Set[PropertyKey] = Set.empty): Eats = {
      val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
      wrapped
    }
  };
  case class Eats(startNode: Animal, rawItem: raw.Relation, endNode: Food) extends Relation[Animal, Food];
  object WholeExampleSchemaWrapping {
    def empty = new WholeExampleSchemaWrapping(raw.Graph.empty);
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
  case class WholeExampleSchemaWrapping(graph: raw.Graph) extends Graph {
    def animals: Seq[Animal] = nodesAs(Animal);
    def foods: Seq[Food] = nodesAs(Food);
    def eats: Seq[Eats] = relationsAs(Eats);
    def nodes: Seq[Node] = Seq.empty.++(animals).++(foods);
    def relations: (Seq[_$1] forSome { 
      type _$1 <: (Relation[_$8, _$5] forSome { 
        type _$8;
        type _$5
      })
    }) = Seq.empty.++(eats);
    def abstractRelations: (Seq[_$10] forSome { 
      type _$10 <: (AbstractRelation[_$7, _$4] forSome { 
        type _$7;
        type _$4
      })
    }) = Seq.empty.++(eats);
    def hyperRelations: (Seq[_$9] forSome { 
      type _$9 <: (HyperRelation[_$6, _$3, _$2, _$12, _$11] forSome { 
        type _$6;
        type _$3;
        type _$2;
        type _$12;
        type _$11
      })
    }) = Seq.empty
  }
}
