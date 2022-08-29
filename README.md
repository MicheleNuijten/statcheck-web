
<p align="center">
  <img width="400px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/statcheck.png"</img>
</p>


# statcheck on the web

statcheck is a "spellchecker" for statistics. It checks whether your p-values match their accompanying test statistic and degrees of freedom. statcheck searches for null-hypothesis significance test (NHST) in APA style (e.g., t(28) = 2.2, p < .05). It recalculates the p-value using the reported test statistic and degrees of freedom. If the reported and computed p-values don't match, statcheck will flag the result as an error.

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-consistent.svg"</img>
</p>

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-inconsistent.svg"</img>
</p>

<p align="center">
  <img max-width="600px" src="https://github.com/MicheleNuijten/statcheck-web/raw/main/www/img/faq-decision-inconsistent.svg"</img>
</p>

# How can I use the statcheck web app?

The statcheck web app is a simple interface that calls on the statcheck R package in the background. To use the web app, all you need is a working internet connection. You don't need R, or even know how to work with R.

Simply upload a paper in .pdf, .html, or .docx format. Statcheck will search your document for statistics and returns a table with all detected statistics and whether they are internally consistent or not. 

# Where can I find more information?

* <a href="https://rpubs.com/michelenuijten/statcheckmanual" target="_blank">The 
        	  manual</a>: A detailed instruction manual with information on what 
        	  statcheck can and cannot do, information on how to install and use 
        	  the statcheck R package, and more.
* <a href="https://github.com/MicheleNuijten/statcheck" target="_blank">The GitHub 
        	  page</a>: Here you can find statcheck's latest developments.
* <a href="http://cran.r-project.org/web/packages/statcheck/" target="_blank">The R 
        	  package</a>: The R package has additional functionality which 
        	  allows you to change more settings and to scan entire folders of 
        	  papers.
* <a href="https://doi.org/10.3758/s13428-015-0664-2" target="_blank">
        	  The paper</a>: The seminal paper in which statcheck was introduced. 
        	  We ran statcheck on over 30,000 psychology papers and report 
        	  general inconsistency-prevalences over time and per journal.
* <a href="https://psyarxiv.com/tcxaj/" target="_blank">The validity study</a>: We 
        	  compared statcheck's performance with manual checks and assessed 
        	  its accuracy in classifying results as consistent/inconsistent

