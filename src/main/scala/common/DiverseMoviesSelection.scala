package common





/*
Diverse Movies
# On boarding recommendations

One of the main goals at Netflix has been to get the right titles in front each of our members at the right time. The problem is when a user first signs up to Netflix, we don't know anything about his/her taste therefore preventing us from delivering the best recommendation experience we are aiming for.

We decided we would ask the newly joined users to give us some hints about what they might want to watch. More specifically, we would show them a short list of titles to pick from in order to jump start the recommendation engine.

Asking our members to choose options from the full catalogue of tens of thousands of titles in alphabetical order would be a poor user experience, so we had to come up with a more streamlined solution.

A good solution to provide the most diverse selection of titles is to use the greedy algorithm for maximum coverage.

# Maximum coverage problem

Given a dataset representing the history of views that fits in memory, return the N most diverse movies:

1. Choose the most viewed movie in the dataset
2. Remove all views from users who viewed the movie found in step 1
3. Repeat, starting at step 1 with filtered dataset from step 2

*/

object DiverseMoviesSelection {

  object model {
    type MovieId = Long
    type UserId  = Long
    case class View(userId: UserId, movieId: MovieId)
  }
  import model._


  def diverseMovies(views: Seq[View], numMovie: Int): Set[MovieId] = {

    def movieWithMostViews(currentViews: Seq[View]): Seq[View] = {
      val groupByMovieIdWithViews = currentViews.groupBy(_.movieId)
      val mostWatchedMovieViews = groupByMovieIdWithViews.mapValues(_.size).maxBy(_._2)
      groupByMovieIdWithViews(mostWatchedMovieViews._1)
    }

    def filterViewsByMostMovieWatchedViews(viewsOfTheTopMovie: Seq[View], currentViews: Seq[View]): Seq[View] = {
      currentViews.filter(view => viewsOfTheTopMovie.map(_.userId).contains(view.userId))
    }

    @scala.annotation.tailrec
    def produceDiverseMovies(currentViews: Seq[View], diversedMovies: Set[MovieId] = Set.empty[MovieId], numberOfMovieSuggestionProcess: Int = numMovie): Set[MovieId] = {
      if(numberOfMovieSuggestionProcess == 0) diversedMovies
      else if(currentViews.size == 1) Set(currentViews.head.movieId)
      else {
        val movieWithViews = movieWithMostViews(currentViews)
        val newViews = filterViewsByMostMovieWatchedViews(movieWithViews, currentViews)
        produceDiverseMovies(newViews, numberOfMovieSuggestionProcess = numberOfMovieSuggestionProcess - 1,
          diversedMovies = diversedMovies ++ Set(movieWithViews.head.movieId))
      }
    }

    produceDiverseMovies(views)
  }


  val views = Seq(
    View(userId = 1L, movieId = 1L),
    View(userId = 2L, movieId = 1L)
  )

  diverseMovies(views, 2).foreach(println)


}