object ExampleSchemaMultipleInheritance {
  import renesca.{graph=>raw};
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[raw.Label, (NodeFactory[_$38] forSome { 
    type _$38 <: Node
  })](scala.Tuple2("ARTICLE", Article), scala.Tuple2("TAG", Tag));
  trait RootNodeTraitFactory[NODE <: Node] {
    val nodeLabels: Set[raw.Label] = Set("ARTICLE", "TAG");
    def nodeLabel(node: raw.Node): raw.Label = nodeLabels.intersect(node.labels).head;
    def factory(node: raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
    def wrap(node: raw.Node) = factory(node).wrap(node)
  };
  trait UuidFactory[NODE <: Uuid] extends NodeFactory[NODE];
  object Uuid extends RootNodeTraitFactory[Uuid] {
    val label = raw.Label("UUID");
    val labels = Set(raw.Label("UUID"))
  };
  trait TimestampFactory[NODE <: Timestamp] extends NodeFactory[NODE];
  object Timestamp extends RootNodeTraitFactory[Timestamp] {
    val label = raw.Label("TIMESTAMP");
    val labels = Set(raw.Label("TIMESTAMP"))
  };
  trait TaggableFactory[NODE <: Taggable] extends NodeFactory[NODE];
  object Taggable extends RootNodeTraitFactory[Taggable] {
    val label = raw.Label("TAGGABLE");
    val labels = Set(raw.Label("TAGGABLE"))
  };
  trait Uuid extends Node {
    def uuid: String = item.properties("uuid").asInstanceOf[StringPropertyValue]
  };
  trait Timestamp extends Node {
    def timestamp: Long = item.properties("timestamp").asInstanceOf[LongPropertyValue]
  };
  trait Taggable extends Node;
  object Article extends UuidFactory[Article] with TimestampFactory[Article] with TaggableFactory[Article] {
    val label = raw.Label("ARTICLE");
    val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TIMESTAMP"), raw.Label("TAGGABLE"));
    def wrap(node: raw.Node) = new Article(node);
    def create(content: String, timestamp: Long = System.currentTimeMillis, uuid: String = java.util.UUID.randomUUID.toString): Article = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.node.properties.update("content", content);
      wrapped.node.properties.update("timestamp", timestamp);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def merge(content: String, timestamp: Long = System.currentTimeMillis, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.node.properties.update("content", content);
      wrapped.node.properties.update("timestamp", timestamp);
      wrapped.node.properties.update("uuid", uuid);
      wrapped
    };
    def matches(content: Option[String] = None, timestamp: Option[Long] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (content.isDefined)
        wrapped.node.properties.update("content", content.get)
      else
        ();
      if (timestamp.isDefined)
        wrapped.node.properties.update("timestamp", timestamp.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.node.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    }
  };
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
  case class Article(node: raw.Node) extends Uuid with Timestamp with Taggable {
    override val label = raw.Label("ARTICLE");
    override val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TIMESTAMP"), raw.Label("TAGGABLE"));
    def rev_categorizes: Set[Tag] = predecessorsAs(Tag, Categorizes);
    def content: String = item.properties("content").asInstanceOf[StringPropertyValue]
  };
  case class Tag(node: raw.Node) extends Uuid {
    override val label = raw.Label("TAG");
    override val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def categorizesArticles: Set[Article] = successorsAs(Article, Categorizes);
    def categorizes: Set[Taggable] = Set.empty.++(categorizesArticles);
    def name: String = item.properties("name").asInstanceOf[StringPropertyValue]
  };
  object Categorizes extends RelationFactory[Tag, Categorizes, Taggable] with AbstractRelationFactory[Tag, Categorizes, Taggable] {
    val relationType = raw.RelationType("CATEGORIZES");
    def wrap(relation: raw.Relation) = Categorizes(Tag.wrap(relation.startNode), relation, Taggable.wrap(relation.endNode));
    def create(startNode: Tag, endNode: Taggable): Categorizes = {
      val wrapped = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
      wrapped
    };
    def merge(startNode: Tag, endNode: Taggable, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Categorizes = {
      val wrapped = wrap(raw.Relation.merge(startNode.node, relationType, endNode.node, merge = merge, onMatch = onMatch));
      wrapped
    };
    def matches(startNode: Tag, endNode: Taggable, matches: Set[PropertyKey] = Set.empty): Categorizes = {
      val wrapped = wrap(raw.Relation.matches(startNode.node, relationType, endNode.node, matches = matches));
      wrapped
    }
  };
  case class Categorizes(startNode: Tag, relation: raw.Relation, endNode: Taggable) extends Relation[Tag, Taggable];
  object Blog {
    def empty = new Blog(raw.Graph.empty)
  };
  case class Blog(graph: raw.Graph) extends Graph {
    def articles: Set[Article] = nodesAs(Article);
    def tags: Set[Tag] = nodesAs(Tag);
    def categorizes: Set[Categorizes] = relationsAs(Categorizes);
    def uuids: Set[Uuid] = Set.empty.++(articles).++(tags);
    def timestamps: Set[Timestamp] = Set.empty.++(articles);
    def taggables: Set[Taggable] = Set.empty.++(articles);
    def uuidRelations: (Set[_$39] forSome { 
      type _$39 <: Relation[Uuid, Uuid]
    }) = Set.empty;
    def timestampRelations: (Set[_$40] forSome { 
      type _$40 <: Relation[Timestamp, Timestamp]
    }) = Set.empty;
    def taggableRelations: (Set[_$41] forSome { 
      type _$41 <: Relation[Taggable, Taggable]
    }) = Set.empty;
    def uuidAbstractRelations: (Set[_$42] forSome { 
      type _$42 <: AbstractRelation[Uuid, Uuid]
    }) = Set.empty;
    def timestampAbstractRelations: (Set[_$43] forSome { 
      type _$43 <: AbstractRelation[Timestamp, Timestamp]
    }) = Set.empty;
    def taggableAbstractRelations: (Set[_$44] forSome { 
      type _$44 <: AbstractRelation[Taggable, Taggable]
    }) = Set.empty;
    def uuidHyperRelations: Set[(HyperRelation[Uuid, _$52, _$51, _$49, Uuid] forSome { 
      type _$52 <: (Relation[Uuid, _$47] forSome { 
        type _$47
      });
      type _$51 <: (HyperRelation[Uuid, _$48, _$46, _$50, Uuid] forSome { 
        type _$48;
        type _$46;
        type _$50
      });
      type _$49 <: (Relation[_$45, Uuid] forSome { 
        type _$45
      })
    })] = Set.empty;
    def timestampHyperRelations: Set[(HyperRelation[Timestamp, _$60, _$59, _$57, Timestamp] forSome { 
      type _$60 <: (Relation[Timestamp, _$55] forSome { 
        type _$55
      });
      type _$59 <: (HyperRelation[Timestamp, _$56, _$54, _$58, Timestamp] forSome { 
        type _$56;
        type _$54;
        type _$58
      });
      type _$57 <: (Relation[_$53, Timestamp] forSome { 
        type _$53
      })
    })] = Set.empty;
    def taggableHyperRelations: Set[(HyperRelation[Taggable, _$68, _$67, _$65, Taggable] forSome { 
      type _$68 <: (Relation[Taggable, _$63] forSome { 
        type _$63
      });
      type _$67 <: (HyperRelation[Taggable, _$64, _$62, _$66, Taggable] forSome { 
        type _$64;
        type _$62;
        type _$66
      });
      type _$65 <: (Relation[_$61, Taggable] forSome { 
        type _$61
      })
    })] = Set.empty;
    def nodes: Set[Node] = Set.empty.++(articles).++(tags);
    def relations: (Set[_$80] forSome { 
      type _$80 <: (Relation[_$73, _$77] forSome { 
        type _$73;
        type _$77
      })
    }) = Set.empty.++(categorizes);
    def abstractRelations: (Set[_$75] forSome { 
      type _$75 <: (AbstractRelation[_$72, _$76] forSome { 
        type _$72;
        type _$76
      })
    }) = Set.empty.++(categorizes);
    def hyperRelations: (Set[_$74] forSome { 
      type _$74 <: (HyperRelation[_$71, _$70, _$69, _$79, _$78] forSome { 
        type _$71;
        type _$70;
        type _$69;
        type _$79;
        type _$78
      })
    }) = Set.empty
  }
}
