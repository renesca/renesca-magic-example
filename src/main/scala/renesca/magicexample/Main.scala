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
    // Simple schema with one Node type and one Relation type
    @macros.GraphSchema
    object Schema {
      @Graph trait Zoo {Nodes(Animal) }
      @Node class Animal {val name: String }
      @Relation class Eats(startNode: Animal, endNode: Animal)
    }

    import Schema._

    db.transaction { tx =>
      val zoo = Zoo.empty
      val snake = Animal.create("snake")
      val dog = Animal.create("dog")
      val eats = Eats.create(snake, dog)
      zoo.add(eats)
      tx.persistChanges(zoo)
    }
  }





  // clear database
  db.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")

  // shut down actor system
  restService.actorSystem.shutdown()
}
