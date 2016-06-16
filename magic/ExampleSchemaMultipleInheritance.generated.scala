object ExampleSchemaMultipleInheritance {
  import renesca.{graph=>raw};
  import renesca.QueryHandler;
  import renesca.schema._;
  import renesca.parameter._;
  import renesca.parameter.implicits._;
  val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(Uuid.labels, UuidMatches), scala.Tuple2(Timestamp.labels, TimestampMatches), scala.Tuple2(Taggable.labels, TaggableMatches), scala.Tuple2(Article.labels, Article), scala.Tuple2(Tag.labels, Tag));
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
  trait TimestampMatchesFactory[+NODE <: Timestamp] extends NodeFactory[NODE] {
    def matchesTimestamp(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): NODE
  };
  trait TimestampFactory[+NODE <: Timestamp] extends NodeFactory[NODE] with TimestampMatchesFactory[NODE];
  object Timestamp extends RootNodeTraitFactory[Timestamp] with TimestampMatchesFactory[Timestamp] {
    val label = TimestampMatches.label;
    val labels = TimestampMatches.labels;
    def matches(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): TimestampMatches = TimestampMatches.matches(timestamp, matches);
    def matchesTimestamp(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): TimestampMatches = this.matches(timestamp, matches)
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
  trait Timestamp extends Node {
    def timestamp: Long = rawItem.properties("timestamp").asInstanceOf[LongPropertyValue]
  };
  trait Taggable extends Node {
    def rev_categorizes: Seq[Tag]
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
  object TimestampMatches extends TimestampMatchesFactory[TimestampMatches] {
    val label = raw.Label("TIMESTAMP");
    val labels = Set(raw.Label("TIMESTAMP"));
    def wrap(node: raw.Node) = new TimestampMatches(node);
    def matches(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): TimestampMatches = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (timestamp.isDefined)
        wrapped.rawItem.properties.update("timestamp", timestamp.get)
      else
        ();
      wrapped
    };
    def matchesTimestamp(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): TimestampMatches = this.matches(timestamp, matches)
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
  object Article extends UuidFactory[Article] with TimestampFactory[Article] with TaggableFactory[Article] {
    val label = raw.Label("ARTICLE");
    val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TIMESTAMP"), raw.Label("TAGGABLE"));
    def wrap(node: raw.Node) = new Article(node);
    def matches(content: Option[String] = None, timestamp: Option[Long] = None, uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.matches(labels, matches = matches));
      if (content.isDefined)
        wrapped.rawItem.properties.update("content", content.get)
      else
        ();
      if (timestamp.isDefined)
        wrapped.rawItem.properties.update("timestamp", timestamp.get)
      else
        ();
      if (uuid.isDefined)
        wrapped.rawItem.properties.update("uuid", uuid.get)
      else
        ();
      wrapped
    };
    def matchesUuid(uuid: Option[String] = None, matches: Set[PropertyKey] = Set.empty): Article = this.matches(None, None, uuid, matches);
    def matchesTimestamp(timestamp: Option[Long] = None, matches: Set[PropertyKey] = Set.empty): Article = this.matches(None, timestamp, None, matches);
    def matchesTaggable(matches: Set[PropertyKey] = Set.empty): Article = this.matches(None, None, None, matches);
    def create(content: String, timestamp: Long = System.currentTimeMillis, uuid: String = java.util.UUID.randomUUID.toString): Article = {
      val wrapped = wrap(raw.Node.create(labels));
      wrapped.rawItem.properties.update("content", content);
      wrapped.rawItem.properties.update("timestamp", timestamp);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    };
    def merge(content: String, timestamp: Long = System.currentTimeMillis, uuid: String = java.util.UUID.randomUUID.toString, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Article = {
      val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
      wrapped.rawItem.properties.update("content", content);
      wrapped.rawItem.properties.update("timestamp", timestamp);
      wrapped.rawItem.properties.update("uuid", uuid);
      wrapped
    }
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
  case class UuidMatches(rawItem: raw.Node) extends Uuid {
    override val label = raw.Label("UUID");
    override val labels = Set(raw.Label("UUID"))
  };
  case class TimestampMatches(rawItem: raw.Node) extends Timestamp {
    override val label = raw.Label("TIMESTAMP");
    override val labels = Set(raw.Label("TIMESTAMP"))
  };
  case class TaggableMatches(rawItem: raw.Node) extends Taggable {
    override val label = raw.Label("TAGGABLE");
    override val labels = Set(raw.Label("TAGGABLE"));
    def rev_categorizes: Seq[Tag] = predecessorsAs(Tag, Categorizes)
  };
  case class Article(rawItem: raw.Node) extends Uuid with Timestamp with Taggable {
    override val label = raw.Label("ARTICLE");
    override val labels = Set(raw.Label("ARTICLE"), raw.Label("UUID"), raw.Label("TIMESTAMP"), raw.Label("TAGGABLE"));
    def rev_categorizes: Seq[Tag] = predecessorsAs(Tag, Categorizes);
    def content: String = rawItem.properties("content").asInstanceOf[StringPropertyValue]
  };
  case class Tag(rawItem: raw.Node) extends Uuid {
    override val label = raw.Label("TAG");
    override val labels = Set(raw.Label("TAG"), raw.Label("UUID"));
    def categorizesArticles: Seq[Article] = successorsAs(Article, Categorizes);
    def categorizes: Seq[Taggable] = Seq.empty.++(categorizesArticles);
    def name: String = rawItem.properties("name").asInstanceOf[StringPropertyValue]
  };
  object Categorizes extends RelationFactory[Tag, Categorizes, Taggable] with AbstractRelationFactory[Tag, Categorizes, Taggable] {
    val relationType = raw.RelationType("CATEGORIZES");
    def wrap(relation: raw.Relation) = Categorizes(Tag.wrap(relation.startNode), relation, Taggable.wrap(relation.endNode));
    def create(startNode: Tag, endNode: Taggable): Categorizes = {
      val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
      wrapped
    };
    def merge(startNode: Tag, endNode: Taggable, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Categorizes = {
      val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
      wrapped
    };
    def matches(startNode: Tag, endNode: Taggable, matches: Set[PropertyKey] = Set.empty): Categorizes = {
      val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
      wrapped
    }
  };
  case class Categorizes(startNode: Tag, rawItem: raw.Relation, endNode: Taggable) extends Relation[Tag, Taggable];
  object WholeExampleSchemaMultipleInheritance {
    def empty = new WholeExampleSchemaMultipleInheritance(raw.Graph.empty);
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
  object Blog {
    def empty = new Blog(raw.Graph.empty);
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
  case class WholeExampleSchemaMultipleInheritance(graph: raw.Graph) extends Graph {
    def articles: Seq[Article] = nodesAs(Article);
    def tags: Seq[Tag] = nodesAs(Tag);
    def categorizes: Seq[Categorizes] = relationsAs(Categorizes);
    def uuids: Seq[Uuid] = Seq.empty.++(articles).++(tags);
    def timestamps: Seq[Timestamp] = Seq.empty.++(articles);
    def taggables: Seq[Taggable] = Seq.empty.++(articles);
    def uuidRelations: (Seq[_$81] forSome { 
      type _$81 <: Relation[Uuid, Uuid]
    }) = Seq.empty;
    def timestampRelations: (Seq[_$82] forSome { 
      type _$82 <: Relation[Timestamp, Timestamp]
    }) = Seq.empty;
    def taggableRelations: (Seq[_$83] forSome { 
      type _$83 <: Relation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidAbstractRelations: (Seq[_$84] forSome { 
      type _$84 <: AbstractRelation[Uuid, Uuid]
    }) = Seq.empty;
    def timestampAbstractRelations: (Seq[_$85] forSome { 
      type _$85 <: AbstractRelation[Timestamp, Timestamp]
    }) = Seq.empty;
    def taggableAbstractRelations: (Seq[_$86] forSome { 
      type _$86 <: AbstractRelation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidHyperRelations: Seq[(HyperRelation[Uuid, _$87, _$89, _$94, Uuid] forSome { 
      type _$87 <: (Relation[Uuid, _$92] forSome { 
        type _$92
      });
      type _$89 <: (HyperRelation[Uuid, _$93, _$91, _$88, Uuid] forSome { 
        type _$93;
        type _$91;
        type _$88
      });
      type _$94 <: (Relation[_$90, Uuid] forSome { 
        type _$90
      })
    })] = Seq.empty;
    def timestampHyperRelations: Seq[(HyperRelation[Timestamp, _$95, _$97, _$102, Timestamp] forSome { 
      type _$95 <: (Relation[Timestamp, _$100] forSome { 
        type _$100
      });
      type _$97 <: (HyperRelation[Timestamp, _$101, _$99, _$96, Timestamp] forSome { 
        type _$101;
        type _$99;
        type _$96
      });
      type _$102 <: (Relation[_$98, Timestamp] forSome { 
        type _$98
      })
    })] = Seq.empty;
    def taggableHyperRelations: Seq[(HyperRelation[Taggable, _$103, _$105, _$110, Taggable] forSome { 
      type _$103 <: (Relation[Taggable, _$108] forSome { 
        type _$108
      });
      type _$105 <: (HyperRelation[Taggable, _$109, _$107, _$104, Taggable] forSome { 
        type _$109;
        type _$107;
        type _$104
      });
      type _$110 <: (Relation[_$106, Taggable] forSome { 
        type _$106
      })
    })] = Seq.empty;
    def nodes: Seq[Node] = Seq.empty.++(articles).++(tags);
    def relations: (Seq[_$111] forSome { 
      type _$111 <: (Relation[_$118, _$115] forSome { 
        type _$118;
        type _$115
      })
    }) = Seq.empty.++(categorizes);
    def abstractRelations: (Seq[_$120] forSome { 
      type _$120 <: (AbstractRelation[_$117, _$114] forSome { 
        type _$117;
        type _$114
      })
    }) = Seq.empty.++(categorizes);
    def hyperRelations: (Seq[_$119] forSome { 
      type _$119 <: (HyperRelation[_$116, _$113, _$112, _$122, _$121] forSome { 
        type _$116;
        type _$113;
        type _$112;
        type _$122;
        type _$121
      })
    }) = Seq.empty
  };
  case class Blog(graph: raw.Graph) extends Graph {
    def articles: Seq[Article] = nodesAs(Article);
    def tags: Seq[Tag] = nodesAs(Tag);
    def categorizes: Seq[Categorizes] = relationsAs(Categorizes);
    def uuids: Seq[Uuid] = Seq.empty.++(articles).++(tags);
    def timestamps: Seq[Timestamp] = Seq.empty.++(articles);
    def taggables: Seq[Taggable] = Seq.empty.++(articles);
    def uuidRelations: (Seq[_$123] forSome { 
      type _$123 <: Relation[Uuid, Uuid]
    }) = Seq.empty;
    def timestampRelations: (Seq[_$124] forSome { 
      type _$124 <: Relation[Timestamp, Timestamp]
    }) = Seq.empty;
    def taggableRelations: (Seq[_$125] forSome { 
      type _$125 <: Relation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidAbstractRelations: (Seq[_$126] forSome { 
      type _$126 <: AbstractRelation[Uuid, Uuid]
    }) = Seq.empty;
    def timestampAbstractRelations: (Seq[_$127] forSome { 
      type _$127 <: AbstractRelation[Timestamp, Timestamp]
    }) = Seq.empty;
    def taggableAbstractRelations: (Seq[_$128] forSome { 
      type _$128 <: AbstractRelation[Taggable, Taggable]
    }) = Seq.empty;
    def uuidHyperRelations: Seq[(HyperRelation[Uuid, _$129, _$131, _$136, Uuid] forSome { 
      type _$129 <: (Relation[Uuid, _$134] forSome { 
        type _$134
      });
      type _$131 <: (HyperRelation[Uuid, _$135, _$133, _$130, Uuid] forSome { 
        type _$135;
        type _$133;
        type _$130
      });
      type _$136 <: (Relation[_$132, Uuid] forSome { 
        type _$132
      })
    })] = Seq.empty;
    def timestampHyperRelations: Seq[(HyperRelation[Timestamp, _$137, _$139, _$144, Timestamp] forSome { 
      type _$137 <: (Relation[Timestamp, _$142] forSome { 
        type _$142
      });
      type _$139 <: (HyperRelation[Timestamp, _$143, _$141, _$138, Timestamp] forSome { 
        type _$143;
        type _$141;
        type _$138
      });
      type _$144 <: (Relation[_$140, Timestamp] forSome { 
        type _$140
      })
    })] = Seq.empty;
    def taggableHyperRelations: Seq[(HyperRelation[Taggable, _$145, _$147, _$152, Taggable] forSome { 
      type _$145 <: (Relation[Taggable, _$150] forSome { 
        type _$150
      });
      type _$147 <: (HyperRelation[Taggable, _$151, _$149, _$146, Taggable] forSome { 
        type _$151;
        type _$149;
        type _$146
      });
      type _$152 <: (Relation[_$148, Taggable] forSome { 
        type _$148
      })
    })] = Seq.empty;
    def nodes: Seq[Node] = Seq.empty.++(articles).++(tags);
    def relations: (Seq[_$153] forSome { 
      type _$153 <: (Relation[_$160, _$157] forSome { 
        type _$160;
        type _$157
      })
    }) = Seq.empty.++(categorizes);
    def abstractRelations: (Seq[_$162] forSome { 
      type _$162 <: (AbstractRelation[_$159, _$156] forSome { 
        type _$159;
        type _$156
      })
    }) = Seq.empty.++(categorizes);
    def hyperRelations: (Seq[_$161] forSome { 
      type _$161 <: (HyperRelation[_$158, _$155, _$154, _$164, _$163] forSome { 
        type _$158;
        type _$155;
        type _$154;
        type _$164;
        type _$163
      })
    }) = Seq.empty
  }
}
