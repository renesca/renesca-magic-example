package renesca.magicexample

// import renesca.parameter._
// import renesca.parameter.implicits._

import renesca.schema.macros
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
  val wholeGraph = db.queryGraph("MATCH (n) RETURN n LIMIT 1")
  if(wholeGraph.nonEmpty) {
    restService.actorSystem.shutdown()
    sys.error("Database is not empty.")
  }


  {
    // Wrapping low level entities
    @macros.GraphSchema
    object ExampleSchemaWrapping {
      @Node class Animal {val name: String }
      @Node class Food {
        val name: String;
        var amount: Long
      }
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

      @Graph trait Zoo {Nodes(Animal, Food) }
    }

    import ExampleSchemaSubgraph._

    val zoo = Zoo(db.queryGraph("MATCH (a:ANIMAL)-[e:EATS]->(f:FOOD) RETURN a,e,f"))
    val elefant = Animal.create("elefant")
    val pizza = Food.create(name = "pizza", amount = 2)
    zoo.add(Eats.create(elefant, pizza))
    println(zoo.animals) // elefant
    println(zoo.relations) // elefant eats pizza (Relations between Nodes are induced)
    db.persistChanges(zoo)
  }

  {
    // Node and Relation traits
    @macros.GraphSchema
    object ExampleSchemaTraits {
      @Node trait Animal {val name: String }

      // Node with labels FISH and ANIMAL
      @Node class Fish extends Animal
      @Node class Dog extends Animal

      @Relation trait Consumes
      @Relation class Eats(startNode: Animal, endNode: Animal) extends Consumes
      @Relation class Drinks(startNode: Animal, endNode: Animal) extends Consumes

      // Zoo contains all Animals (Animal expands to all subNodes)
      @Graph trait Zoo {Nodes(Animal) }
    }

    import ExampleSchemaTraits._

    val zoo = Zoo.empty
    val bello = Dog.create("bello")
    val wanda = Fish.create("wanda")

    zoo.add(bello)
    zoo.add(wanda)

    zoo.add(Eats.create(bello, wanda))
    zoo.add(Drinks.create(wanda, bello))

    println(zoo.animals) // bello and wanda
    //TODO: there are no accessors for relation traits in graph ?!
    //println(zoo.consumes)
  }


  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.actorSystem.shutdown()
}
