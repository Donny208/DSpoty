#' @title Get Playlist Tracks
#' @name get_playlist_tracks
#' @description This function returns all the tracks of a playlist.
#' @author Donovan Wright
#' @param playlist_id String of artist name.
#' @param offset Integer indicating the offset of the first artist to return. Defaults to 0 (Spotify's API default value).
#' @param access_token Spotify Web API token. Defaults to DSpoty::get_spotify_access_token().
#' @return
#' Returns a data frame with all the tracks of the selected playlist.
#' @export

get_playlist_tracks<-function(playlist_id, limit = 100, offset = 0, access_token = DSpoty::get_spotify_access_token()){

    res1<-RETRY('GET',
                url = str_glue(paste(c("https://api.spotify.com/v1/playlists/",playlist_id,"/tracks/"), collapse = "")),
                query = list(market="US",fields="items(track(name%2Cid))", limit = limit, offset = offset, access_token= access_token),
                quiet = FALSE) %>%
      content %>%
      .$tracks

  print(tracks)
  tracks<-map_df(seq_len(length(res1)), function(this_row){

    track<-res1[[this_row]]

    list(
      track_name = track$name,
      track_uri = track$id,

    )
  })

  return(tracks)

}
