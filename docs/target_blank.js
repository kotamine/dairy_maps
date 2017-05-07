var links = document.querySelectorAll( 'a' );
    for( var i=0, limit=links.length; i<limit; i++ ) {
        if(links[i].href.indexOf(location.hostname)===-1){
        links[i].setAttribute("target", "_blank");
      }
    }