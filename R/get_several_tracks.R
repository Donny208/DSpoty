#' @title Get Several Tracks
#' @name get_several_tracks
#' @description This function allows you to load several tracks in a faster way.
#' @author Alberto Almui?a
#' @param tracks_id_df Dataframe containing the tracks ids in one of the columns.
#' @param ids_label Integer indicating the column number of the tracks ids. Default to 1.
#' @param access_token Spotify Web API token. Defaults to DSpoty::get_spotify_access_token()
#' @return
#' Returns a data frame with all the artist's information
#' @export
#' @examples
#' \dontrun{
#' get_several_tracks(df,1)
#' }


get_several_tracks<-function(playlist_id, limit = 100, offset = 0, access_token = DSpoty::get_spotify_access_token()){

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
