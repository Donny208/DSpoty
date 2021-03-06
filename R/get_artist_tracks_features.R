#' @title Get Artist Tracks Features
#' @name get_artist_tracks_features
#' @description This function returns audio features for all the tracks of a given artist
#' @author Alberto Almui?a
#' @param artist_name String of an artist name
#' @param offset Integer indicating the offset of the first artist to return. Defaults to 0 (Spotify's API default value).
#' @param limit Integer indicating the max number of artists to return. Defaults to 100, max of 100.
#' @param access_token Spotify Web API token. Defaults to DSpoty::get_spotify_access_token()
#' @return
#' Returns a data frame with the tracks features for all the tracks of an artist
#' @export
#' @examples
#' \dontrun{
#' tracks_features<-get_artist_tracks_features('Shakira')
#' }

get_artist_tracks_features<-function(artist_name, limit = 100, offset = 0, access_token = DSpoty::get_spotify_access_token()){

  if(is.character(artist_name)==F){

    stop(str_glue('The parameter artist_name has to be a string. The value introduced is "{artist_name}'))
  }

  tracks_uris<-DSpoty::get_artist_tracks(artist_name) %>% .$track_uri %>% split(., ceiling(seq_along(.)/100)) %>%
    lapply(., paste, collapse = ',')

  res<-lmap(tracks_uris, function(x){

    res1<-RETRY('GET',
                str_glue('https://api.spotify.com/v1/audio-features/?ids={x}'),
                query = list(limit=limit,
                             offset = offset,
                             access_token = access_token),
                quiet = TRUE,
                times = 10) %>% content %>% .$audio_features
  })


  if(all(lapply(res,is.na) %>% unlist()) == F & !all(lapply(res, is.null) %>% unlist())){

    tracks_features<- map_df(seq_len(length(res)), function(this_truck){

      track_info<-res[[this_truck]]

      list(
        track_uri = track_info$id,
        danceability = track_info$danceability,
        energy = track_info$energy,
        key = track_info$key,
        loudness = track_info$loudness,
        mode = track_info$mode,
        speechiness = track_info$speechiness,
        acousticness = track_info$acousticness,
        instrumentalness = track_info$instrumentalness,
        liveness = track_info$liveness,
        valence = track_info$valence,
        tempo = track_info$tempo,
        time_signature = track_info$time_signature,
        popularity = get_track_popularity(track_info$id)

      )
    })

    return(tracks_features)

  }


}
