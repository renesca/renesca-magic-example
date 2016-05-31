package renesca.magicexample

// import renesca.parameter._
// import renesca.parameter.implicits._

import renesca.schema.macros
import renesca.parameter.implicits._
import renesca.{DbService, RestService, Transaction}
import spray.http.BasicHttpCredentials

object Main extends App {
  // set up database connection
  val credentials = BasicHttpCredentials("neo4j", "neo4j")
  // RestService contains an ActorSystem to handle HTTP communication via spray-client
  val restService = new RestService("http://localhost:7474", Some(credentials))

  // query interface for submitting single requests
  val db = new DbService
  // dependency injection
  db.restService = restService

  // only proceed if database is available and empty
  val wholeGraph = db.queryWholeGraph
  if(wholeGraph.nonEmpty) {
    restService.actorSystem.shutdown()
    sys.error("Database is not empty.")
  }


  {
    // Wrapping low level entities
    @macros.GraphSchema
    object ExampleSchemaWrapping {
      // Nodes get their class name as uppercase label
      @Node class Animal {val name: String }
      @Node class Food {
        val name: String
        var amount: Long
      }
      // Relations get their class name as uppercase relationType
      @Relation class Eats(startNode: Animal, endNode: Food)
    }

    import ExampleSchemaWrapping._

    val snake = Animal.create("snake")
    val cake = Food.create(name = "cake", amount = 1000)
    val eats = Eats.create(snake, cake)

    cake.amount -= 100
  }


  {
    // Wrapping induced subgraphs
    @macros.GraphSchema
    object ExampleSchemaSubgraph {
      @Node class Animal {val name: String }
      @Node class Food {
        val name: String
        var amount: Long
      }
      @Relation class Eats(startNode: Animal, endNode: Food)

      // Relations between specified nodes will be induced
      @Graph trait Zoo {Nodes(Animal, Food) }
    }

    import ExampleSchemaSubgraph._

    val zoo = Zoo(db.queryGraph("MATCH (a:ANIMAL)-[e:EATS]->(f:FOOD) RETURN a,e,f"))
    val elefant = Animal.create("elefant")
    val pizza = Food.create(name = "pizza", amount = 2)
    zoo.add(Eats.create(elefant, pizza))
    zoo.animals // Set(elefant)
    zoo.relations // Set(elefant eats pizza)
    db.persistChanges(zoo)
  }

  {
    // Node and Relation traits
    @macros.GraphSchema
    object ExampleSchemaTraits {
      // Inheriting Nodes receive their name as additional label
      @Node trait Animal {val name: String }

      // Node with labels FISH and ANIMAL
      @Node class Fish extends Animal
      @Node class Dog extends Animal

      @Relation trait Consumes { val funny:Boolean }

      // Relations can connect Node traits.
      // So we can connect any node extending the trait
      @Relation class Eats(startNode: Animal, endNode: Animal) extends Consumes
      @Relation class Drinks(startNode: Animal, endNode: Animal) extends Consumes

      // Zoo contains all Animals (Animal expands to all subNodes)
      @Graph trait Zoo {Nodes(Animal) }
    }

    import ExampleSchemaTraits._

    val zoo = Zoo.empty
    // merge dog and fish on the name property
    // (creates the animal if it does not exist, otherwise the existing animal is matched)
    val bello = Dog.merge(name = "bello", merge = Set("name"))
    val wanda = Fish.merge(name = "wanda", merge = Set("name"))

    zoo.add(bello)
    zoo.add(wanda)
    zoo.animals // Set(bello, wanda)

    zoo.add(Eats.create(bello, wanda, funny = false))
    zoo.add(Drinks.create(wanda, bello, funny = true))
  }

  {
    // Multiple inheritance, default property values
    @macros.GraphSchema
    object ExampleSchemaMultipleInheritance {
      // Assignments are default values for properties
      // They can also be arbitrary statements
      @Node trait Uuid { val uuid: String = java.util.UUID.randomUUID.toString }
      @Node trait Timestamp { val timestamp: Long = System.currentTimeMillis }
      @Node trait Taggable

      @Node class Article extends Uuid with Timestamp with Taggable {
        val content:String
      }
      @Node class Tag extends Uuid { val name:String }
      @Relation class Categorizes(startNode:Tag, endNode:Taggable)

      @Graph trait Blog {Nodes(Article, Tag)}
    }

    import ExampleSchemaMultipleInheritance._

    val initGraph = Blog.empty
    initGraph.add(Tag.create(name = "useful"))
    initGraph.add(Tag.create(name = "important"))
    db.persistChanges(initGraph)

    val blog = Blog.empty

    // match the previously created tags
    blog.add(Tag.matches(name = Some("useful"), matches = Set("name")))
    blog.add(Tag.matches(name = Some("important"), matches = Set("name")))

    // automatically set uuid and timestamp
    val article = Article.create(content = "Some useful and important content")
    blog.add(article)

    // set all tags on the article
    blog.tags.foreach{ tag =>
      blog.add(Categorizes.create(tag, article))
    }

    blog.taggables // Set(article)
    article.rev_categorizes // blog.tags

    db.persistChanges(blog)
  }

  {
    // Hyperrelations
    @macros.GraphSchema
    object ExampleSchemaHyperRelations {
      @Node trait Uuid { val uuid: String = java.util.UUID.randomUUID.toString }
      @Node trait Taggable
      @Node class Tag extends Uuid {val name:String}
      @Node class User extends Uuid {val name:String}
      @Node class Article extends Uuid with Taggable {val content:String}

      // A HyperRelation is a node representing a relation:
      // (n)-[]->(hyperRelation)-[]->(m)
      // It behaves like node and relation at the same time
      // and therefore can extend node and relation traits.
      @HyperRelation class Tagged(startNode: Tag, endNode: Taggable) extends Uuid
      // Because they are nodes, we can connect a HyperRelation with another Node
      @Relation class Supports(startNode: User, endNode: Tagged)
    }

    import ExampleSchemaHyperRelations._
    val user = User.create(name="pumuckl")
    val helpful = Tag.create(name="helpful")
    val article = Article.create(content="Dog eats Snake")

    val tags = Tagged.create(helpful, article) // HyperRelation
    val supports = Supports.create(user, tags) // Relation from user to HyperRelation
  }

  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.actorSystem.shutdown()
}
