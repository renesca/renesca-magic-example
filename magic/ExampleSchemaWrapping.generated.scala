object ExampleSchemaWrapping {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$1] forSome { 
    type _$1 <: Node
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
    };
    def merge(name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Animal = {
      val node = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      node.node.properties.update("name", name);
      node
    };
    def matches(name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Animal = {
      val node = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        node.node.properties.update("name", name.get)
      else
        ();
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
    };
    def merge(amount: Long, name: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Food = {
      val node = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      node.node.properties.update("amount", amount);
      node.node.properties.update("name", name);
      node
    };
    def matches(amount: Option[Long] = None, name: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Food = {
      val node = wrap(raw.Node.matches(labels, matches = matches));
      if (amount.isDefined)
        node.node.properties.update("amount", amount.get)
      else
        ();
      if (name.isDefined)
        node.node.properties.update("name", name.get)
      else
        ();
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
    };
    def merge(startNode: Animal, endNode: Food, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Eats = {
      val relation = wrap(raw.Relation.merge(startNode.node, relationType, endNode.node, merge = merge, onMatch = onMatch));
      relation
    };
    def matches(startNode: Animal, endNode: Food, matches: Set[PropertyKey] = Set.empty): Eats = {
      val relation = wrap(raw.Relation.matches(startNode.node, relationType, endNode.node, matches = matches));
      relation
    }
  };
  case class Eats(startNode: Animal, relation: raw.Relation, endNode: Food) extends Relation[Animal, Food]
}
