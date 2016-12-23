object ExampleSchemaHyperRelations {
  import renesca.{graph=>raw};
  import renesca.QueryHandler;
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(Uuid.labels, UuidMatches), scala.Tuple2(Taggable.labels, TaggableMatches), scala.Tuple2(Tag.labels, Tag), scala.Tuple2(User.labels, User), scala.Tuple2(Article.labels, Article), scala.Tuple2(Tagged.labels, Tagged));
  trait RootNodeTraitFactory[+NODE <: Node] {
    def factory(node: raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  def setupDbConstraints(queryHandler: QueryHandler) = ();
  trait UuidMatchesFactory[+NODE <: Uuid] extends NodeFactory[NODE] {
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
  };
  trait UuidFactory[+NODE <: Uuid] extends NodeFactory[NODE] with UuidMatchesFactory[NODE];
  object Uuid extends RootNodeTraitFactory[Uuid] with UuidMatchesFactory[Uuid] {
    val label = UuidMatches.label;
    val labels = UuidMatches.labels;
    def matches(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): UuidMatches = UuidMatches.matches(uuid, matches);
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): UuidMatches = this.matches(uuid, matches)
  };
  trait TaggableMatchesFactory[+NODE <: Taggable] extends NodeFactory[NODE] {
    def matchesTaggable(matches: Set[PropertyKey] = Set.empty): NODE
  };
  trait TaggableFactory[+NODE <: Taggable] extends NodeFactory[NODE] with TaggableMatchesFactory[NODE];
  object Taggable extends RootNodeTraitFactory[Taggable] with TaggableMatchesFactory[Taggable] {
    val label = TaggableMatches.label;
    val labels = TaggableMatches.labels;
    def matches(matches: Set[PropertyKey] = Set.empty): TaggableMatches = TaggableMatches.matches(matches);
    def matchesTaggable(matches: Set[PropertyKey] = Set.empty): TaggableMatches = this.matches(matches)
  };
  trait Uuid extends Node {
    def uuid: String = rawItem.properties("uuid").asInstanceOf[StringPropertyValue]
  };
  trait Taggable extends Node {
    def rev_taggeds: Seq[Tag]
  };
  object UuidMatches extends UuidMatchesFactory[UuidMatches] {
    val label = raw.Label("UUID");
    val labels = Set(raw.Label("UUID"));
    def wrap(node: raw.Node) = new UuidMatches(node);
    def matches(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): UuidMatches = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (uuid.isDefined)
        wrapped.rawItem.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): UuidMatches = this.matches(uuid, matches)
  };
  object TaggableMatches extends TaggableMatchesFactory[TaggableMatches] {
    val label = raw.Label("TAGGABLE");
    val labels = Set(raw.Label("TAGGABLE"));
    def wrap(node: raw.Node) = new TaggableMatches(node);
    def matches(matches: Set[PropertyKey] = Set.empty): TaggableMatches = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      wrapped
    };
    def matchesTaggable(matches: Set[PropertyKey] = Set.empty): TaggableMatches = this.matches(matches)
  };
  object Tag extends UuidFactory[Tag] {
    val label = raw.Label("TAG");
    val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def wrap(node: raw.Node) = new Tag(node);
    def matches(name: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tag = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.rawItem.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tag = this.matches(None, uuid, matches);
    def create(name: String, uuid: String = java.util.UUID.randomUUID.toString): Tag = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("name", name);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    };
    def merge(name: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Tag = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("name", name);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    }
  };
  object User extends UuidFactory[User] {
    val label = raw.Label("USER");
    val labels = Set(raw.Label("USER"), raw.Label("UUID"));
    def wrap(node: raw.Node) = new User(node);
    def matches(name: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): User = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (name.isDefined)
        wrapped.rawItem.properties.update("name", name.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.rawItem.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): User = this.matches(None, uuid, matches);
    def create(name: String, uuid: String = java.util.UUID.randomUUID.toString): User = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("name", name);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    };
    def merge(name: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): User = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("name", name);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    }
  };
  object Article extends UuidFactory[Article] with TaggableFactory[Article] {
    val label = raw.Label("ARTICLE");
    val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TAGGABLE"));
    def wrap(node: raw.Node) = new Article(node);
    def matches(content: Option[String] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (content.isDefined)
        wrapped.rawItem.properties.update("content", content.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.rawItem.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = this.matches(None, uuid, matches);
    def matchesTaggable(matches: Set[PropertyKey] = Set.empty): Article = this.matches(None, None, matches);
    def create(content: String, uuid: String = java.util.UUID.randomUUID.toString): Article = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("content", content);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    };
    def merge(content: String, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("content", content);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    }
  };
  case class UuidMatches(rawItem: raw.Node) extends Uuid {
    override val label = raw.Label("UUID");
    override val labels = Set(raw.Label("UUID"))
  };
  case class TaggableMatches(rawItem: raw.Node) extends Taggable {
    override val label = raw.Label("TAGGABLE");
    override val labels = Set(raw.Label("TAGGABLE"));
    def rev_taggeds: Seq[Tag] = predecessorsAs(Tag, Tagged)
  };
  case class Tag(rawItem: raw.Node) extends Uuid {
    override val label = raw.Label("TAG");
    override val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def taggedArticles: Seq[Article] = successorsAs(Article, Tagged);
    def taggeds: Seq[Taggable] = Seq.empty.++(taggedArticles);
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class User(rawItem: raw.Node) extends Uuid {
    override val label = raw.Label("USER");
    override val labels = Set(raw.Label("USER"), raw.Label("UUID"));
    def supports: Seq[Tagged] = successorsAs(Tagged, Supports);
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue]
  };
  case class Article(rawItem: raw.Node) extends Uuid with Taggable {
    override val label = raw.Label("ARTICLE");
    override val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TAGGABLE"));
    def rev_taggeds: Seq[Tag] = predecessorsAs(Tag, Tagged);
    def content: String = rawItem.properties("content").asInstanceOf[StringPropertyValue]
  };
  object Supports extends RelationFactory[User, Supports, Tagged] with AbstractRelationFactory[User, Supports, Tagged] {
    val relationType = raw.RelationType("SUPPORTS");
    def wrap(relation: raw.Relation) = Supports(User.wrap(relation.startNode), relation, Tagged.wrap(relation.endNode));
    def create(startNode: User, endNode: Tagged): Supports = {
      val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
      wrapped
    };
    def merge(startNode: User, endNode: Tagged, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Supports = {
      val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
      wrapped
    };
    def matches(startNode: User, endNode: Tagged, matches: Set[PropertyKey] = Set.empty): Supports = {
      val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
      wrapped
    }
  };
  case class Supports(startNode: User, rawItem: raw.Relation, endNode: Tagged) extends Relation[User, Tagged];
  object Tagged extends HyperRelationFactory[Tag, TaggedStart, Tagged, TaggedEnd, Taggable] with UuidFactory[Tagged] {
    override val label = raw.Label("TAGGED");
    override val labels = Set(raw.Label("TAGGED"), raw.Label("UUID"));
    override val startRelationType = raw.RelationType("TAGGEDSTART");
    override val endRelationType = raw.RelationType("TAGGEDEND");
    override def wrap(node: raw.Node) = new Tagged(node);
    override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation) = {
      val hyperRelation = wrap(middleNode);
      hyperRelation._startRelation = TaggedStart(Tag.wrap(startRelation.startNode), startRelation, hyperRelation);
      hyperRelation._endRelation = TaggedEnd(hyperRelation, endRelation, Taggable.wrap(endRelation.endNode));
      hyperRelation
    };
    def create(startNode: Tag, endNode: Taggable, uuid: String = java.util.UUID.randomUUID.toString): Tagged = {
      val middleNode = raw.Node.create(labels);
      middleNode.properties.update("uuid", uuid);
      wrap(raw.Relation.create(startNode.rawItem, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.rawItem))
    };
    def merge(startNode: Tag, endNode: Taggable, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Tagged = {
      val middleNode = raw.Node.merge(labels, merge = merge, onMatch = onMatch);
      middleNode.properties.update("uuid", uuid);
      wrap(raw.Relation.merge(startNode.rawItem, startRelationType, middleNode), middleNode, raw.Relation.merge(middleNode, endRelationType, endNode.rawItem))
    };
    def matches(startNode: Tag, endNode: Taggable, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tagged = {
      val middleNode = raw.Node.matches(labels, matches = matches);
      if (uuid.isDefined)
        middleNode.properties.update("uuid", uuid.get)
      else
        ();
      wrap(raw.Relation.matches(startNode.rawItem, startRelationType, middleNode), middleNode, raw.Relation.matches(middleNode, endRelationType, endNode.rawItem))
    };
    def matchesNode(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tagged = {
      val middleNode = raw.Node.matches(labels, matches = matches);
      if (uuid.isDefined)
        middleNode.properties.update("uuid", uuid.get)
      else
        ();
      wrap(middleNode)
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Tagged = this.matchesNode(uuid, matches)
  };
  object TaggedEnd extends RelationFactory[Tagged, TaggedEnd, Taggable] {
    val relationType = raw.RelationType("TAGGEDEND");
    def wrap(relation: raw.Relation) = TaggedEnd(Tagged.wrap(relation.startNode), relation, Taggable.wrap(relation.endNode))
  };
  object TaggedStart extends RelationFactory[Tag, TaggedStart, Tagged] {
    val relationType = raw.RelationType("TAGGEDSTART");
    def wrap(relation: raw.Relation) = TaggedStart(Tag.wrap(relation.startNode), relation, Tagged.wrap(relation.endNode))
  };
  case class Tagged(rawItem: raw.Node) extends HyperRelation[Tag, TaggedStart, Tagged, TaggedEnd, Taggable] with Uuid {
    override val label = raw.Label("TAGGED");
    override val labels = Set(raw.Label("TAGGED"), raw.Label("UUID"));
    def rev_supports: Seq[User] = predecessorsAs(User, Supports)
  };
  case class TaggedStart(startNode: Tag, rawItem: raw.Relation, endNode: Tagged) extends Relation[Tag, Tagged];
  case class TaggedEnd(startNode: Tagged, rawItem: raw.Relation, endNode: Taggable) extends Relation[Tagged, Taggable];
  object WholeExampleSchemaHyperRelations {
    def empty = new WholeExampleSchemaHyperRelations(raw.Graph.empty);
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
  case class WholeExampleSchemaHyperRelations(graph: raw.Graph) extends Graph {
    def tags: Seq[Tag] = nodesAs(Tag);
    def users: Seq[User] = nodesAs(User);
    def articles: Seq[Article] = nodesAs(Article);
    def supports: Seq[Supports] = relationsAs(Supports);
    def taggeds: Seq[Tagged] = hyperRelationsAs(Tagged);
    def uuids: Seq[Uuid] = Seq.empty.++(tags).++(users).++(articles).++(taggeds);
    def taggables: Seq[Taggable] = Seq.empty.++(articles);
    def uuidRelations: (Seq[_$165] forSome { 
      type _$165 <: Relation[Uuid, Uuid]
    }) = Seq.empty.++(supports);
    def taggableRelations: (Seq[_$166] forSome { 
      type _$166 <: Relation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidAbstractRelations: (Seq[_$167] forSome { 
      type _$167 <: AbstractRelation[Uuid, Uuid]
    }) = Seq.empty.++(supports);
    def taggableAbstractRelations: (Seq[_$168] forSome { 
      type _$168 <: AbstractRelation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidHyperRelations: Seq[(HyperRelation[Uuid, _$169, _$176, _$175, Uuid] forSome { 
      type _$169 <: (Relation[Uuid, _$173] forSome { 
        type _$173
      });
      type _$176 <: (HyperRelation[Uuid, _$174, _$172, _$170, Uuid] forSome { 
        type _$174;
        type _$172;
        type _$170
      });
      type _$175 <: (Relation[_$171, Uuid] forSome { 
        type _$171
      })
    })] = Seq.empty;
    def taggableHyperRelations: Seq[(HyperRelation[Taggable, _$177, _$184, _$183, Taggable] forSome { 
      type _$177 <: (Relation[Taggable, _$181] forSome { 
        type _$181
      });
      type _$184 <: (HyperRelation[Taggable, _$182, _$180, _$178, Taggable] forSome { 
        type _$182;
        type _$180;
        type _$178
      });
      type _$183 <: (Relation[_$179, Taggable] forSome { 
        type _$179
      })
    })] = Seq.empty;
    def nodes: Seq[Node] = Seq.empty.++(tags).++(users).++(articles).++(taggeds);
    def relations: (Seq[_$186] forSome { 
      type _$186 <: (Relation[_$192, _$195] forSome { 
        type _$192;
        type _$195
      })
    }) = Seq.empty.++(supports);
    def abstractRelations: (Seq[_$194] forSome { 
      type _$194 <: (AbstractRelation[_$191, _$189] forSome { 
        type _$191;
        type _$189
      })
    }) = Seq.empty.++(supports).++(taggeds);
    def hyperRelations: (Seq[_$193] forSome { 
      type _$193 <: (HyperRelation[_$190, _$188, _$187, _$185, _$196] forSome { 
        type _$190;
        type _$188;
        type _$187;
        type _$185;
        type _$196
      })
    }) = Seq.empty.++(taggeds)
  }
}
