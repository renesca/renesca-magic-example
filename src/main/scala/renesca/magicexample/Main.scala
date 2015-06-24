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
    
    val animal = Animal.create("snake")
    val food = Food.create(name = "cake", amount = 1000)
    val eats = Eats.create(animal, food)

    food.amount -= 100
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
    zoo.add(elefant)
    println(zoo.animals)
    db.persistChanges(zoo)
  }



  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.actorSystem.shutdown()
}
