$(document).ready(function(){
    /*Sets the default sate of the page.*/
    current_tab = "";
    authenticated = true;
    selectedpost = null;
    type_selection = [1, 2, 3, 4];
    category_selection = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
    orderselection = ['','','','','','','',''];
    page_offset = 0;
    /*This is the initial push state.*/
    history.replaceState({current_tab: current_tab,
                        current_page: current_page,
                        call_type: 'tab'}, null, current_page);
    
    /*Call the server to load the categories page.*/
    GotoTab(current_tab, current_page, false);
    
    /*This function is called when the page is
    sent forward or back.*/
    window.onpopstate = function (event) {
        if (event.state === null) {
            return;
        }
        $tab = event.state.current_tab;
        $page = event.state.current_page;
        $call_type = event.state.call_type;
        current_tab = $tab;
        current_page = $page;
        if($call_type === 'tab') GotoTab($tab, $page, false);
        if($call_type === 'profile') GotoProfile(event.state.user, false);
        if($call_type === 'edit') GotoEdit($tab, $page, false, event.state.type);
        if($call_type === 'my-posts') GotoMyPosts(event.state.table, false);
        if($call_type === 'user-questions') GotoUserPosts(event.state.user, event.state.table, false);
        if($call_type === 'search') GotoSearch(event.state.criteria, false);
        $(".menu-link").parent("li").attr("class", "");
        $("#menu-" + $tab).parent("li").attr("class", "active");
    };
    
    $( document ).ajaxComplete(function() {
        SetUserElements();
        if(current_page === 'post-list' /*||
                current_page === 'user-questions'*/) {
            $('.professional').show();
            $('.category').show();
        } else {
            $('.professional').hide();
            $('.category').hide();
        }
        if(current_page === 'post-list' ||
                current_page === 'question' ||
                current_page === 'user-questions') {
            $('.new-post').show();
        } else {
            $('.new-post').hide();
        }
    });
    
    /*A simpler way to call ajax.*/
    function CallPage($pageid, $data) {
        $url = 'php/' + $pageid + '.php';
        $.ajax({url: $url,
                type: 'POST',
                data: $data,
                success: function(data){
                    $("#content-body").html(data);
                }});
        RefreshTopPosts();
        return false;
    }
    
    function RefreshTopPosts($data) {
        $url = 'php/top-posts.php';
        $.ajax({url: $url,
                type: 'POST',
                data: $data,
                success: function(data){
                    $("#top-posts").html(data);
                }});
    }
    
    function CallQuery($data) {
        $.ajax({url: 'php/query.php',
                type: 'POST',
                data: $data,
                success: function(data){
                    log(data);
                }});
        return false;
    }     
    
    function PostEditor($postid, $type, $recipient) {
        $data = {postid: $postid,
                recipient: $recipient,
                type: $type};
        $.ajax({url: 'php/post-editor.php',
                type: 'POST',
                data: $data,
                success: function(data){
                    $('.modal-content').html(data);
                }});        
        return false;
    }
    
    /*Function to load new page content into the content-body div.*/
    function GotoTab($tabid, $pageid, $saveState) {
        $data = {tabid: $tabid,
                page_offset: page_offset,
                type_selection: type_selection,
                category_selection: category_selection,
                orderby: orderselection};
        CallPage($pageid, $data);        
        if($saveState) {
            current_tab = $tabid;
            current_page = $pageid;
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                call_type: 'tab'}, $tabid, $tabid);
        }
        window.scrollTo(0, 0);        
        return false;
    }
    
    function GotoProfile($user, $saveState) {
        $data = {user: $user};
        CallPage('profile', $data);    
        if($saveState) {
            current_tab = $user;
            current_page = 'profile';
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                user: $user,
                call_type: 'profile'}, $user, $user);
        }
        window.scrollTo(0, 0);        
        return false;
    }
    
    function GotoEdit($postid, $pageid, $saveState, $type) {
        $data = {postid: $postid,
                postid: selectedpost,
                type: $type};
        selectedquestion = current_tab;
        CallPage($pageid, $data);    
        if($saveState) {
            current_tab = $postid;
            current_page = $pageid;
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                call_type: 'edit',
                type: $type}, $postid, $postid);
        }        
        hideSidebar();
        window.scrollTo(0, 0);        
        return false;
    }
    
    function GotoMyPosts($table, $saveState) {
        $tabid = 'my-' + $table;
        $data = {table: $table,
                page_offset: page_offset,
                type_selection: type_selection,
                category_selection: category_selection,
                orderby: orderselection};
        CallPage('my-posts', $data);
        if($saveState) {
            current_tab = $tabid;
            current_page = 'my-posts';
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                call_type: 'my-posts',
                table: $table}, $tabid, $tabid);
        }
        hideSidebar();
        window.scrollTo(0, 0);        
        return false;
    }
    
    function GotoUserPosts($user, $table, $saveState) {
        $tabid = $user + '-' + $table;
        $data = {user: $user,
                table: $table,
                page_offset: page_offset,
                type_selection: type_selection,
                category_selection: category_selection,
                orderby: orderselection};
        CallPage('user-questions', $data);
        if($saveState) {
            current_tab = $tabid;
            current_page = 'user-questions';
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                call_type: 'user-questions',
                user: $user,
                table: $table}, $tabid, $tabid);
        }
        hideSidebar();
        window.scrollTo(0, 0);        
        return false;
    }
    
    function GotoSearch($criteria, $saveState) {
        $tabid = 'search';
        $data = {tabid: $tabid,
                criteria: $criteria,
                page_offset: page_offset,
                type_selection: type_selection,
                category_selection: category_selection,
                orderby: orderselection};
        CallPage('search', $data);
        if($saveState) {
            current_tab = $tabid;
            current_page = 'search';
            history.pushState({current_tab: current_tab,
                current_page: current_page,
                call_type: 'search',
                criteria: $criteria}, $tabid, $tabid);
        }
        hideSidebar();
        window.scrollTo(0, 0);        
        return false;
    }
    
    function DeletePost($id, $table) {        
        $data = {postid: $id,
                table: $table,
                type: "delete-post"};
        CallQuery($data);
        GotoTab("post-list", "post-list", true);
        hideSidebar();
        window.scrollTo(0, 0);        
        return false;
    }
    
    function RemoveUserElements(){
        /*$(".login-only").detach();*/
    }
    
    function SetUserElements() {
        $(".login-only").each( function() {
            if($(this).data('creator') !== $(this).data('user')) {
                $(this).detach();
            }
        });
    }
    
    /*Function to hide the sidebar.*/
    function hideSidebar() {
        $("#main-sidebar").css("width", "0");
        $("#content-wrapper").css("margin-left", "0");
        return false;
    };
    
    /*Function to toggle the sidebar.*/
    $(".sidebar-toggle").click(function() {
        if($("#main-sidebar").width() === 180) {
            $("#main-sidebar").css("width", "0");
            $("#content-wrapper").css("margin-left", "0");
        } else {
            $("#main-sidebar").css("width", "180px");
            $("#content-wrapper").css("margin-left", "180px");
        }
    });
    
    /*Function to call the new post dialog.*/
    $("body").on("click", "#add-question", (function() {
        PostEditor("", "add-question");
        $('#modal_new_post').modal('show');
    })); 
    
   $("body").on("click", "#search-btn", (function() {
        $criteria = $("#search-text").val();
        GotoSearch($criteria, true);
    }));
    
    $('body').on('keypress', "#search-text", (function (e) {
        if(e.which === 13){
            $criteria = $("#search-text").val();
            GotoSearch($criteria, true);
        }
    }));
    
    $("body").on("click", "#edit-link", (function() {
        $type = $(this).data('type');
        selectedpost = $(this).data('postid');
        PostEditor(selectedpost, $type);
        $('#modal_new_post').modal('show');
    }));
    
    $("body").on("click", "#delete-link", (function() {
        var answer = confirm('Are you sure you want to delete this post?');
        if (answer) {
          console.log('yes');
          DeletePost($(this).data('postid'), $(this).data('table'));     
        }
        else
        {
          console.log('cancel');
        }           
    }));
    
    $("body").on("click", ".add-answer", (function() {
        selectedpost = $(this).data('postid');
        PostEditor(selectedpost, "add-answer");
        $('#modal_new_post').modal('show');
    }));
    
    $("body").on("click", ".msg-tab-btn", (function() {
        $tab = $(this).data("tab");        
        if($tab === "inbox") {
            $closetab = "sent";
        } else {
            $closetab = "inbox";
        }
        $("#" + $closetab).css("display", "none");
        $("#" + $tab).css("display", "block");
    }));
    
    $("body").on("click", ".send-message", (function() {
        $recipient = $(this).data('recipient');
        PostEditor("", "send-message", $recipient);
        $('#modal_new_post').modal('show');
    }));
    
    $("body").on("click", ".send-reply", (function() {
        $recipient = $(this).data('recipient');
        $replyid = $(this).data('replyid');
        PostEditor($replyid, "send-message", $recipient);
        $('#modal_new_post').modal('show');
    }));
    
    /*Function for links to load pages into the content-body div.*/
    $("body").on("click", ".tab-link", (function(){
        $tabid = $(this).data("tabid");
        $pageid = $(this).data("pageid");
        GotoTab($tabid, $pageid, true);
    }));
    
    $("body").on("click", ".menu-link", (function() {
        $tabid = $(this).data("tabid");
        $pageid = $(this).data("pageid");
        GotoTab($tabid, $pageid, true);
        $(".menu-link").parent("li").attr("class", "");
        $(this).parent("li").attr("class", "active");
    }));
    
    $("body").on("click", ".profile-link", (function(){
        $user = $(this).data('user');
        GotoProfile($user, true);
    }));
    
    $("body").on("click", ".my-posts-link", (function(){
        $user = $(this).data("user");
        $table = $(this).data("table");
        GotoUserPosts($user, $table, true);
    }));
    
    $("body").on("click", ".user-posts-link", (function(){
        $user = $(this).data("user");
        $table = $(this).data("table");
        GotoUserPosts($user, $table, true);
    }));
    
    /*Function for links that are used to select posts to view.*/
    $("body").on("click", ".post-link", (function(){
        $postid = $(this).data("postid");
        $pageid = $(this).data("pageid");
        $data = { postid: $postid,
            type: "add-view"};
        CallQuery($data);
        GotoTab($postid, $pageid, true);
    }));
    
    /*A universal function to perform operations
    based on the type of data being submitted.
    This will be expanded to include answer posts, and editing posts.*/
    $("body").on("click", "#submit", (function(){
        $type = $(this).data("type");
        $body = $("#post-body").summernote('code');
        $user = $(this).data("user");
        switch($type) {
            case "add-question":
                $subcategory = $("#submit-category").val();
                $question = $("#submit-question").val();
                current_tab = '';
                current_page = 'post-list';
                $data = { subcategory: $subcategory,
                    question : $question,
                    body : $body,
                    type: "add-question"};
                break;
            case "edit-question":
                $question = $("#submit-question").val();
                current_tab = $(this).data('postid');
                current_page = 'question';
                $data = {postid: selectedpost,
                    question : $question,
                    body : $body,
                    type: "edit-question"};
                break;
            case "add-answer":
                current_tab = $(this).data('postid');
                current_page = 'question';
                $data = { postid: selectedpost,
                    body : $body,
                    type: "add-answer"};
                break;
            case "edit-answer":
                $id = $(this).data('postid');             
                $data = {postid: selectedpost,
                    body : $body,
                    type: "edit-answer"};
                break;
            case "send-message":
                $recipient = $("#submit-recipient").text();
                $replyid = $(this).data('postid');
                current_page = 'my-posts';
                $data = { recipient: $recipient,
                    postid: $replyid,
                    body : $body,
                    type: "send-message"};
                break;
            /*default:
                default code block*/
        }
        CallQuery($data);
        //alert(current_tab + "|" + current_page)
        if($type === "send-message") {
            GotoUserPosts($user, "user_messages", true);
        } else {
            GotoTab(current_tab, current_page, true);
        }        
    }));
    
    $("body").on("click", "#feedback-submit", (function(){
        $body = $("#feedback-body").summernote('code');
        current_tab = '';
        current_page = 'post-list';
        $data = { body : $body,
            type: "feedback"};
        CallQuery($data);
        GotoTab(current_tab, current_page, true);
        $("#menu-feedback").parent("li").attr("class", "");
        $("#menu-post-list").parent("li").attr("class", "active");
    }));
    
    $("body").on("change", "#drop_professional", (function(){
        type_selection = $(this).val();
        if(current_page === "user-questions") {
            $user = $(this).data("user");
            $table = $(this).data("table");
            GotoUserPosts($user, $table, false);
        } else {
            GotoTab(current_tab, current_page, false);
        }        
    }));
    
    $("body").on("change", "#drop_category", (function(){
        category_selection = $(this).val();
        if(current_page === "user-questions") {
            $user = $(this).data("user");
            $table = $(this).data("table");
            GotoUserPosts($user, $table, false);
        } else {
            GotoTab(current_tab, current_page, false);
        }    
    }));
    
    $("body").on("change", "#order-by", (function(){
        orderselection = ['','','','','','','',''];
        orderselection[$(this).prop('selectedIndex')] = 'selected';
        if(current_page === "user-questions") {
            $user = $(this).data("user");
            $table = $(this).data("table");
            GotoUserPosts($user, $table, false);
        } else {
            GotoTab(current_tab, current_page, false);
        }   
    }));
    
    $("body").on("click", "#previous-page", (function(){
        page_offset -= 20;
        if(page_offset < 0) page_offset = 0;
        GotoTab(current_tab, current_page, true);        
        return false;
    }));
    
    $("body").on("click", ".select-page", (function(){
        page_offset = $(this).data('offset');
        GotoTab(current_tab, current_page, true);        
        return false;
    }));
    
    $("body").on("click", "#next-page", (function(){
        page_offset += 20;
        if(page_offset > $(this).data('max')) {
            page_offset -= 20;
        }
        GotoTab(current_tab, current_page, true);        
        return false;
    }));
    
    /*Function uses to toggle the icon for the
    vote button, and to send data for votes to the database.*/
    $("body").on("click", ".question-vote", (function(){
        $postid = $(this).data('postid');
        if($(this).attr("class") === "question-vote fa fa-arrow-up login-only"){
            $(this).attr("class","question-vote fa fa-close login-only");
            $type = "add-vote";
            $votes = parseInt($('#votes' + $postid).text()) + 1;
        } else {
            $(this).attr("class","question-vote fa fa-arrow-up login-only");
            $type = "remove-vote";
            $votes = parseInt($('#votes' + $postid).text()) - 1;
        }
        $data = { table: 'questions',
            postid: $postid,
            type: $type};
        CallQuery($data);
        $('#votes' + $postid).text($votes);
    }));
    
    /*Function uses to toggle the icon for the
    vote button, and to send data for votes to the database.*/
    $("body").on("click", ".answer-vote", (function(){
        $postid = $(this).data('postid');
        if($(this).attr("class") === "answer-vote fa fa-arrow-up login-only"){
            $(this).attr("class","answer-vote fa fa-close login-only");
            $type = "add-vote";
            $votes = parseInt($('#a-votes' + $postid).text()) + 1;
        } else {
            $(this).attr("class","answer-vote fa fa-arrow-up login-only");
            $type = "remove-vote";
            $votes = parseInt($('#a-votes' + $postid).text()) - 1;
        }
        $data = { table: 'answers',
            postid: $postid,
            type: $type};
        CallQuery($data);
        $('#a-votes' + $postid).text($votes);
    }));
    
    $("body").on("click", ".accept-answer", (function(){
        $answerid = $(this).data('answerid');
        $questionid = $(this).data('questionid');
        $data = { postid: $answerid,
            type: 'accept-answer'};
        CallQuery($data);
        GotoTab($questionid, 'question', true);
        
    }));
    
    function SummernoteInit() {
        $('#post-body').summernote({
            height: 250,
            toolbar: [
              // [groupName, [list of button]]
              ['style', ['bold', 'italic', 'underline', 'clear']],
              ['font', ['strikethrough', 'superscript', 'subscript']],
              ['fontsize', ['fontsize']],
              ['color', ['color']],
              ['para', ['ul', 'ol', 'paragraph']],
              ['height', ['height']]
            ]
        });
    }    
    
    $('#modal_new_post').on('shown.bs.modal', function() {
        //alert("fawoop");
        SummernoteInit();
    });
    
    $("body").on("click", "#follow", (function(){
        if( $(this).text() === "Unfollow"){
            $(this).text("Follow");
        } else { 
            $(this).text("Unfollow");
        }
        $user = $(this).data('user');
        $data = { user: $user,
            type: 'follow'};
        CallQuery($data);
    }));
    
    $("body").on("click", ".star1", (function(){
        $postid = $(this).data('postid');
        $table = $(this).data('table');
        $voteid = $table + $postid
        $(this).attr('class', 'star1 glyphicon glyphicon-star');
        $('#' + $voteid + 'star2').attr('class', 'star2 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star3').attr('class', 'star3 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star4').attr('class', 'star4 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star5').attr('class', 'star5 glyphicon glyphicon-star-empty');
        $data = { table: $table,
            postid: $postid,
            type: 'set-stars',
            starcount: 1};
        CallQuery($data);
        return false;
    }));
    
    $("body").on("click", ".star2", (function(){
        $postid = $(this).data('postid');
        $table = $(this).data('table');
        $voteid = $table + $postid
        $('#' + $voteid + 'star1').attr('class', 'star1 glyphicon glyphicon-star');
        $(this).attr('class', 'star2 glyphicon glyphicon-star');        
        $('#' + $voteid + 'star3').attr('class', 'star3 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star4').attr('class', 'star4 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star5').attr('class', 'star5 glyphicon glyphicon-star-empty');
        $data = { table: $table,
            postid: $postid,
            type: 'set-stars',
            starcount: 2};
        CallQuery($data);
        return false;
    }));
    
    $("body").on("click", ".star3", (function(){
        $postid = $(this).data('postid');
        $table = $(this).data('table');
        $voteid = $table + $postid
        $('#' + $voteid + 'star1').attr('class', 'star1 glyphicon glyphicon-star');
        $('#' + $voteid + 'star2').attr('class', 'star2 glyphicon glyphicon-star');
        $(this).attr('class', 'star3 glyphicon glyphicon-star');
        $('#' + $voteid + 'star4').attr('class', 'star4 glyphicon glyphicon-star-empty');
        $('#' + $voteid + 'star5').attr('class', 'star5 glyphicon glyphicon-star-empty');
        $data = { table: $table,
            postid: $postid,
            type: 'set-stars',
            starcount: 3};
        CallQuery($data);
        return false;
    }));
    
    $("body").on("click", ".star4", (function(){
        $postid = $(this).data('postid');
        $table = $(this).data('table');
        $voteid = $table + $postid
        $('#' + $voteid + 'star1').attr('class', 'star1 glyphicon glyphicon-star');
        $('#' + $voteid + 'star2').attr('class', 'star2 glyphicon glyphicon-star');
        $('#' + $voteid + 'star3').attr('class', 'star3 glyphicon glyphicon-star');
        $(this).attr('class', 'star4 glyphicon glyphicon-star');
        $('#' + $voteid + 'star5').attr('class', 'star5 glyphicon glyphicon-star-empty');
        $data = { table: $table,
            postid: $postid,
            type: 'set-stars',
            starcount: 4};
        CallQuery($data);
        return false;
    }));
    
    $("body").on("click", ".star5", (function(){
        $postid = $(this).data('postid');
        $table = $(this).data('table');
        $voteid = $table + $postid
        $('#' + $voteid + 'star1').attr('class', 'star1 glyphicon glyphicon-star');
        $('#' + $voteid + 'star2').attr('class', 'star2 glyphicon glyphicon-star');
        $('#' + $voteid + 'star3').attr('class', 'star3 glyphicon glyphicon-star');
        $('#' + $voteid + 'star4').attr('class', 'star4 glyphicon glyphicon-star');
        $(this).attr('class', 'star5 glyphicon glyphicon-star');
        $data = { table: $table,
            postid: $postid,
            type: 'set-stars',
            starcount: 5};
        CallQuery($data);
        return false;
    }));
    
    $("body").on("click", "#edit-profile", (function(){
        GotoTab("", "questionnaire", true);
    }));
    
    $("body").on("click", "#questionnaire-continue", (function(){        
        if ($('#hide-profile').is(":checked")) {
            $hideProfile = 1;
        } else {
            $hideProfile = 0;
        }
        $profiletype = $('#profile-type').val();
        $answers = $('#questionnaire1').val();
        for(i = 2; i < 10; i++) {
            $answers = $answers + ',' + $('#questionnaire' + i).val();
        }
        $user = $(this).data("user");
        $data = {type: 'set-questionnaire',
            user: $user,
            hide_profile: $hideProfile,
            profile_type: $profiletype,
            answers: $answers};
        CallQuery($data);
        GotoTab("","my-profile", true);
    }));
    
    /*For testing purposes.*/
    $("body").on("click", "#test-btn", (function(){
        
    }));
}); 