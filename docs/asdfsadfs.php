<!DOCTYPE html>
<html lang="en">

    <head>

        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="description" content="">
        <meta name="author" content="">

        <title>Moo Talk - Robot Users</title>

        <!-- Bootstrap Core CSS -->
        <link href="vendor/bootstrap/css/bootstrap.min.css" rel="stylesheet">
	<link rel="stylesheet" href="css/2015-tc.css">
		<!--[if IE 8]>
			<link rel="stylesheet" type="text/css" href="css/ie8.css">
		<![endif]-->
		<!--	Script below controls dropdown toggle and footer accordion. -->
		<script src="js/umnhf-2015.js" type="text/javascript"></script>
		<script src="js/html5shiv-printshiv.js" type="text/javascript"></script>

        <!-- Custom Fonts -->
        <link href="vendor/font-awesome/css/font-awesome.min.css" rel="stylesheet" type="text/css">
        <link href="https://fonts.googleapis.com/css?family=Montserrat:400,700" rel="stylesheet" type="text/css">
        <link href='https://fonts.googleapis.com/css?family=Kaushan+Script' rel='stylesheet' type='text/css'>
        <link href='https://fonts.googleapis.com/css?family=Droid+Serif:400,700,400italic,700italic' rel='stylesheet' type='text/css'>
        <link href='https://fonts.googleapis.com/css?family=Roboto+Slab:400,100,300,700' rel='stylesheet' type='text/css'>

        <!-- Theme CSS -->
        <link href="css/agency.css" rel="stylesheet">
        
        <style>
            
            .logout_icon{
                
                width: 50px;
                height: 50px;
                border-radius: 50%;
                margin-top: -20px;
            }
            
        </style>

        <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
        <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
        <!--[if lt IE 9]>
            <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
            <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
        <![endif]-->

    </head>

    <body id="page-top" class="index">
        <!-- Navigation -->


        <nav id="mainNav" class="navbar navbar-default navbar-custom navbar-fixed-top" style="background-color: #7a0019; width: 100%;">
            <div class="container">
 
		<div class="umnhf" id="umnhf-h" role="banner">
			<!-- Skip Links: Give your nav and content elements the appropriate ID attributes -->
			<div id="skipLinks"><a href="#main-nav">Main navigation</a><a href="#main-content">Main content</a></div>
			<div class="printer"><div class="left"></div><div class="right"><strong>University of Minnesota</strong><br />http://twin-cities.umn.edu/<br />612-625-5000</div></div>
			<div class="umnhf" id="umnhf-h-mast">
				<a class="umnhf" id="umnhf-h-logo" href="http://twin-cities.umn.edu/" style="margin-left: 0px;"><span>Go to the U of M home page</span></a>
				 
				 
			</div>
 

			 
		</div>
                <!-- Brand and toggle get grouped for better mobile display -->
                <div class="navbar-header page-scroll">
                    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar_links">
                        <span class="sr-only">Toggle navigation</span> Menu <i class="fa fa-bars"></i>
                    </button>
                    <a class="navbar-brand page-scroll" href="#page-top">Moo Talk - Robot Users</a>
                </div>

                <!-- Collect the nav links, forms, and other content for toggling -->
                <div class="collapse navbar-collapse" id="navbar_links">
                    <ul class="nav navbar-nav navbar-right">
                        <li class="hidden">
                            <a href="#page-top"></a>
                        </li>
                        <li>
                            <a class="page-scroll" href="#services">How to get started</a>
                        </li>                   
                        <li>
                            <a class="page-scroll" href="#team">Mission</a>                            
                        </li>
                        <li>
                            <a class="page-scroll" href="#about">About</a>
                        </li>  
                        <li>
                            <?php 
                            
                            if(isset($user_profile->email)){
                              echo   '<a href="logout.php">Log out   <img class="logout_icon" src="' . $user_profile->photoURL . '"/></a>';
                            } else {
                                echo '<a href="sigin.php">Sign in</a>';
                            }
                            
                            ?>
                            
                        </li> 

                    </ul>
                </div>
                <!-- /.navbar-collapse -->
            </div>
            <!-- /.container-fluid -->
        </nav>

        <!-- Header -->
        <header>
            <div class="container">
                <div class="intro-text">
                    <div class="intro-lead-in">Welcome to Moo Talk - Robot Users!</div>
                    <div class="intro-heading">Where ideas come together!</div>
                    <a href="#services" class="page-scroll btn btn-xl">Get started now !</a>
                </div>
            </div>
        </header>

        <!-- Services Section -->
        <section id="services" class="bg-light-gray">
            <div class="container">
                <div class="row">
                    <div class="col-lg-12 text-center">
                        <h2 class="section-heading">How to get started?</h2>
                        <h3 class="section-subheading text-muted">Just sign in using your account from Google or Facebook.</h3>
                    </div>
                </div>
                <div class="row text-center">
                    <div class="col-md-12">
                        <span class="fa-stack fa-4x">
                            <i class="fa fa-circle fa-stack-2x text-primary"></i>
                            <a href="sigin.php"><i class="fa fa-sign-in fa-stack-1x fa-inverse"></i></a>
                        </span>
                        <h4 class="service-heading">Sign in</h4>
                        <p class="text-muted">Your personal information WILL NOT be shared with any third parties.</p>
                    </div>

                </div>
            </div>
        </section>
        
        <!-- Team Section -->
        <section id="team" class="bg-light-gray">
            <div class="container">
                <div class="row">
                    <div class="col-lg-12 text-center">
                        <h2 class="section-heading">Our mission</h2>
                       <h3 class="section-subheading text-muted">
                           This website promotes professional networking and discussions on robotic milking systems, serving as a creative platform that grows with the users and their needs.
                       </h3>
                    </div>
                </div>
               
                <div class="row">
                    <div class="col-lg-8 col-lg-offset-2 text-center">
                        <p class="large text-muted"></p>
                    </div>
                </div>
            </div>
        </section>

        <!-- About Section -->
        <section id="about">
            <div class="container">
                <div class="row">
                    <div class="col-lg-12 text-center">
                        <h2 class="section-heading">About</h2>
                        <!-- <h3 class="section-subheading text-muted">Lorem ipsum dolor sit amet consectetur.</h3> -->
                    </div>
                </div>
                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="section-subheading text-muted">
                            Welcome to Moo Talk – Robot Users. This is a professional networking site for dairy producers, industry specialists, and extension agents for discussing robotic milking systems. <br><br>
                            The transition into a robotic milking system involves many new challenges and opportunities. Developing your professional network is a key to stay informed, troubleshoot, and make the most of this new technology. Stay active and connected via Moo Talk – Robot Users.<br><br>                            
                            This site is maintained by <a href="http://www.extension.umn.edu/agriculture/dairy/">Dairy Extension at the University of Minnesota</a>.
                        </h3>
                    </div>
                </div>
            </div>
        </section>
        
        <footer id="umnhf-f" class="umnhf" role="contentinfo">
            <nav id="umnhf-f-myu">
                <h3 class="umnhf-f-title visually-hidden">For Students, Faculty, and Staff</h3>
                <ul>
                    <li><a href="http://onestop.umn.edu/">One Stop</a></li>
                    <li><a href="https://www.myu.umn.edu/">My U <span></span></a></li>
                </ul>
            </nav>
            <small>&copy; <span id="cdate"><?php echo date("Y"); ?></span> Regents of the University of Minnesota. All rights reserved. The University of Minnesota is an equal opportunity educator and employer. <a href="http://privacy.umn.edu">Privacy Statement</a></small>
         
        </footer>
      

        <!-- jQuery -->
        <script src="vendor/jquery/jquery.min.js"></script>

        <!-- Bootstrap Core JavaScript -->
        <script src="vendor/bootstrap/js/bootstrap.min.js"></script>

        <!-- Plugin JavaScript -->
        <script src="http://cdnjs.cloudflare.com/ajax/libs/jquery-easing/1.3/jquery.easing.min.js"></script>

        <!-- Contact Form JavaScript -->
        <script src="js/jqBootstrapValidation.js"></script>
        <script src="js/contact_me.js"></script>

        <!-- Theme JavaScript -->
        <script src="js/agency.min.js"></script>

 

    </body>

</html>
