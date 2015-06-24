object ExampleSchemaHyperRelations {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$81] forSome { 
    type _$81 <: Node
  })](scala.Tuple2("TAG", Tag), scala.Tuple2("USER", User), scala.Tuple2("ARTICLE", Article), scala.Tuple2("TAGS", Tags));
  trait RootNodeTraitFactory[NODE <: Node] {
    val nodeLabels: Set[raw.Label] = Set("TAG", "USER", "ARTICLE", "TAGS");
    def nodeLabel(node: raw.Node): raw.Label = nodeLabels.intersect(node.labels).head;
    def factory(node: raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  trait UuidFactory[NODE <: Uuid] extends NodeFactory[NODE];
  object Uuid extends RootNodeTraitFactory[Uuid] {
    val label = raw.Label("UUID");
    val labels = Set(raw.Label("UUID"))
  };
  trait TaggableFactory[NODE <: Taggable] extends NodeFactory[NODE];
  object Taggable extends RootNodeTraitFactory[Taggable] {
    val label = raw.Label("TAGGABLE");
    val labels = Set(raw.Label("TAGGABLE"))
  };
  trait Uuid extends Node {
    def uuid: String = item.properties("uuid").asInstanceOf[StringPropertyValue]
  };
  trait Taggable extends Node;
  object Tag extends UuidFactory[Tag] {
    val label = raw.Label("TAG");
    val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def wrap(node: raw.Node) = new Tag(node);
    def create(name: String, uuid: String = java.util.UUID.randomUUID.toString): Tag = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.node.properties.update("name", name);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def merge(name: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Tag = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.node.properties.update("name", name);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def matches(name: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tag = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.node.properties.update("name", name.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.node.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    }
  };
  object User extends UuidFactory[User] {
    val label = raw.Label("USER");
    val labels = Set(raw.Label("USER"), raw.Label("UUID"));
    def wrap(node: raw.Node) = new User(node);
    def create(name: String, uuid: String = java.util.UUID.randomUUID.toString): User = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.node.properties.update("name", name);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def merge(name: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): User = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.node.properties.update("name", name);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def matches(name: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): User = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.node.properties.update("name", name.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.node.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    }
  };
  object Article extends UuidFactory[Article] with TaggableFactory[Article] {
    val label = raw.Label("ARTICLE");
    val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TAGGABLE"));
    def wrap(node: raw.Node) = new Article(node);
    def create(content: String, uuid: String = java.util.UUID.randomUUID.toString): Article = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.node.properties.update("content", content);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def merge(content: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.node.properties.update("content", content);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def matches(content: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (content.isDefined)
        wrapped.node.properties.update("content", content.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.node.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    }
  };
  case class Tag(node: raw.Node) extends Uuid {
    override val label = raw.Label("TAG");
    override val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def tagsArticles: Set[Article] = successorsAs(Article, Tags);
    def tags: Set[Taggable] = Set.empty.++(tagsArticles);
    def name: String = item.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class User(node: raw.Node) extends Uuid {
    override val label = raw.Label("USER");
    override val labels = Set(raw.Label("USER"), raw.Label("UUID"));
    def supports: Set[Tags] = successorsAs(Tags, Supports);
    def name: String = item.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class Article(node: raw.Node) extends Uuid with Taggable {
    override val label = raw.Label("ARTICLE");
    override val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TAGGABLE"));
    def rev_tags: Set[Tag] = predecessorsAs(Tag, Tags);
    def content: String = item.properties("content").asInstanceOf[StringPropertyValue]
  };
  object Supports extends RelationFactory[User, Supports, Tags] with AbstractRelationFactory[User, Supports, Tags] {
    val relationType = raw.RelationType("SUPPORTS");
    def wrap(relation: raw.Relation) = Supports(User.wrap(relation.startNode), relation, Tags.wrap(relation.endNode));
    def create(startNode: User, endNode: Tags): Supports = {
      val wrapped = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      wrapped
    };
    def merge(startNode: User, endNode: Tags, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Supports = {
      val wrapped = wrap(raw.Relation.merge(startNode.node, relationType, endNode.node, merge = merge, onMatch = onMatch));
      wrapped
    };
    def matches(startNode: User, endNode: Tags, matches: Set[PropertyKey] = Set.empty): Supports = {
      val wrapped = wrap(raw.Relation.matches(startNode.node, relationType, endNode.node, matches = matches));
      wrapped
    }
  };
  case class Supports(startNode: User, relation: raw.Relation, endNode: Tags) extends Relation[User, Tags];
  object Tags extends HyperRelationFactory[Tag, TagToTags, Tags, TagsToTaggable, Taggable] with UuidFactory[Tags] {
    override val label = raw.Label("TAGS");
    override val labels = Set(raw.Label("TAGS"));
    override val startRelationType = raw.RelationType("TAGTOTAGS");
    override val endRelationType = raw.RelationType("TAGSTOTAGGABLE");
    override def wrap(node: raw.Node) = new Tags(node);
    override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation) = {
      val hyperRelation = wrap(middleNode);
      hyperRelation._startRelation = TagToTags(Tag.wrap(startRelation.startNode), startRelation, hyperRelation);
      hyperRelation._endRelation = TagsToTaggable(hyperRelation, endRelation, Taggable.wrap(endRelation.endNode));
      hyperRelation
    };
    def create(startNode: Tag, endNode: Taggable, uuid: String = java.util.UUID.randomUUID.toString): Tags = {
      val middleNode = raw.Node.create(labels);
      middleNode.properties.update("uuid", uuid);
      val wrapped = wrap(raw.Relation.create(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.node));
      wrapped.path = raw.Path(wrapped.startRelation.relation, wrapped.endRelation.relation).right.toOption;
      wrapped
    };
    def merge(startNode: Tag, endNode: Taggable, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Tags = {
      val middleNode = raw.Node.merge(labels, merge = merge, onMatch = onMatch);
      middleNode.properties.update("uuid", uuid);
      val wrapped = wrap(raw.Relation.merge(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.merge(middleNode, endRelationType, endNode.node));
      wrapped.path = raw.Path(wrapped.startRelation.relation, wrapped.endRelation.relation).right.toOption;
      wrapped
    };
    def matches(startNode: Tag, endNode: Taggable, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tags = {
      val middleNode = raw.Node.matches(labels, matches = matches);
      if (uuid.isDefined)
        middleNode.properties.update("uuid", uuid.get)
      else
        ();
      val wrapped = wrap(raw.Relation.matches(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.matches(middleNode, endRelationType, endNode.node));
      wrapped.path = raw.Path(wrapped.startRelation.relation, wrapped.endRelation.relation).right.toOption;
      wrapped
    }
  };
  case class Tags(node: raw.Node) extends HyperRelation[Tag, TagToTags, Tags, TagsToTaggable, Taggable] with Uuid {
    override val label = raw.Label("TAGS");
    override val labels = Set(raw.Label("TAGS"), raw.Label("UUID"))
  };
  case class TagToTags(startNode: Tag, relation: raw.Relation, endNode: Tags) extends Relation[Tag, Tags];
  case class TagsToTaggable(startNode: Tags, relation: raw.Relation, endNode: Taggable) extends Relation[Tags, Taggable]
}
