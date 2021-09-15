package bidImplimentation

import akka.actor.{ActorSystem, Actor, Props, ActorRef}

object EskimiTechnicalTask extends App{

    //companion object for BidAgent
    object BidAgent{
        import CampaignManager.{Impression, User, Site}
        case class CampaignMatcher(imp:Impression, user:User, site: Site)
    
    }

    //Bid Agent Actor
    class BidAgent extends Actor{
        //importing objects
        import BidAgent._
        import CampaignManager._
        override def receive: Receive = {
            case BidRequest(id, imp, site, user, device) => 
                println("Checking bid request")
                print(s"We recieved a bid request from $sender")
                val campaignActor = context.actorOf(Props[Campaigns], "campaignsActor")
                campaignActor ! CampaignMatcher(imp, user, site)    
            case _ => println("This is not a bid request")
                }
    }

    object CampaignManager{
        case class StartCampaign(bidRef: ActorRef)
        case class BidRequest(id: String, imp: Impression, site: Site, user: User, device: Device)
        case class Impression(id: String, wmin: Int, wmax: Int, w: Int, hmin:Int, hmax: Int, h: Int, bidFloor: Double)
        case class Site(id: String, domain: String)
        case class User(id: String, geo: Geo)
        case class Device(id: String, geo:Geo)
        case class Geo(country: String)
        val myBidRequest = BidRequest(
            "SGu1Jpq1IO",
            Impression("1", 50, 300, 300, 100, 300, 250, 3.12123),
            Site("0006a522ce0f4bbbbaa6b3c38cafaa0f", "fake.tld"),
            User("USARIO1", Geo("LT")),
            Device("440579f4b408831516ebd02f6e1c31b4", Geo("LT"))
        )
    }

     class CampaignManager extends Actor{
         import CampaignManager._
         import BidAgent._
        override def receive: Receive = {
            case StartCampaign(bidRef) => 
                bidRef ! myBidRequest
            case _ => println("something")
        }
  }

object Campaigns{
  case class Campaign(id: Int, country: String, targeting: Targeting, banners: List[Banner], bid: Double)
  case class Targeting(targetedSiteIds: Seq[String])
  case class Banner(id: Int, src: String, width: Int, height: Int)
  case class BidResponse(id: String, bidRequestId: String, price: Double, adid: String, banner: Banner)
val responseBid = BidResponse(
    "response1", 
    "SGu1Jpq1IO", 
    3.12123, 
    "1", 
    Banner(
        1, 
        "https://business.eskimi.com/wp-content/uploads/2020/06/openGraph.jpeg",
         300,
         250)
         )

val activeCampaigns = Seq(
 Campaign(
   id = 1,
   country = "LT",
   targeting = Targeting(
     targetedSiteIds = Seq("0006a522ce0f4bbbbaa6b3c38cafaa0f") // Use collection of your choice
   ),
   banners = List(
     Banner(
       id = 1,
       src = "https://business.eskimi.com/wp-content/uploads/2020/06/openGraph.jpeg",
       width = 300,
       height = 250
     )
   ),
   bid = 5d
 )
)
}

class Campaigns extends Actor{
        import BidAgent._
        import CampaignManager._
        import Campaigns._
        override def receive: Receive = {
            case CampaignMatcher(imp, user, site) =>
            //   println(s"imp:$imp, user:$user, site:$site from campaigns")
            activeCampaigns.map{ac =>
            ac.targeting.targetedSiteIds.map{siteString =>
                if(siteString == site.id){
                        println("site match ...")
                    if(ac.country == user.geo.country){
                        println("country match ....")
                        if((ac.bid).toFloat >= imp.bidFloor){
                            println("bid successfull ...")
                            if(ac.banners.head.width == imp.w && ac.banners.head.height == imp.h){
                                println("size matched ...")
                                println("...........................................")
                                println(responseBid)
                                
                            }else{
                                println("Sorry no campaigns matching banner size")
                            }
                        }else{
                            println("Sorry no campaigns matching bid")
                        }
                    }else{
                        println("Sorry no campaigns matching country")
                    }
                }else{
                    println("Sorry no campaigns matching site")
                }
            }
                }
            case _ => println("This is not a bid request")
                }
    }

import CampaignManager.StartCampaign
 val system = ActorSystem("bidSystem")
 val bidActor = system.actorOf(Props[BidAgent], "bidActor")
 val campaignManagerActor = system.actorOf(Props[CampaignManager], "campaignManagerActor")

campaignManagerActor ! StartCampaign(bidActor)

}