% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Design.R
\name{Design}
\alias{Design}
\title{Design class}
\description{
Design class

Design class
}
\details{
A class for clinical trial designs.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{n_sim}}{number of simulation}

\item{\code{sample_size}}{the sample size at final analysis}

\item{\code{stratum_name}}{the name of strata}

\item{\code{stratum_ratio}}{the ratio between strata}

\item{\code{enroll}}{number of simulation}

\item{\code{failure}}{number of simulation}

\item{\code{dropout}}{number of simulation}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Design-validate}{\code{Design$validate()}}
\item \href{#method-Design-new}{\code{Design$new()}}
\item \href{#method-Design-input_stratum}{\code{Design$input_stratum()}}
\item \href{#method-Design-input_enroll}{\code{Design$input_enroll()}}
\item \href{#method-Design-input_failure}{\code{Design$input_failure()}}
\item \href{#method-Design-input_dropout}{\code{Design$input_dropout()}}
\item \href{#method-Design-clone}{\code{Design$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-validate"></a>}}
\if{latex}{\out{\hypertarget{method-Design-validate}{}}}
\subsection{Method \code{validate()}}{
Validate input parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$validate(n_sim, sample_size)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n_sim}}{number of simulation}

\item{\code{sample_size}}{sample size}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-new"></a>}}
\if{latex}{\out{\hypertarget{method-Design-new}{}}}
\subsection{Method \code{new()}}{
initialize the design/simulation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$new(n_sim, sample_size)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n_sim}}{number of simulation}

\item{\code{sample_size}}{the sample size at final analysis}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Design} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-input_stratum"></a>}}
\if{latex}{\out{\hypertarget{method-Design-input_stratum}{}}}
\subsection{Method \code{input_stratum()}}{
Input enrollment rate over time
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$input_stratum(name, ratio)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{the label/name of the strata}

\item{\code{ratio}}{the prevalence ratio between strata}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Design} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-input_enroll"></a>}}
\if{latex}{\out{\hypertarget{method-Design-input_enroll}{}}}
\subsection{Method \code{input_enroll()}}{
Input enrollment rate over time
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$input_enroll(dist)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dist}}{R6 class to define the enrollment distribution}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Design} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-input_failure"></a>}}
\if{latex}{\out{\hypertarget{method-Design-input_failure}{}}}
\subsection{Method \code{input_failure()}}{
Input failure rate over time
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$input_failure(arm, stratum, dist)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{arm}}{arm label}

\item{\code{stratum}}{stratum label}

\item{\code{dist}}{R6 class to define the enrollment distribution}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Design} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-input_dropout"></a>}}
\if{latex}{\out{\hypertarget{method-Design-input_dropout}{}}}
\subsection{Method \code{input_dropout()}}{
Input dropout rate over time
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$input_dropout(arm, stratum, dist)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{arm}}{arm label}

\item{\code{stratum}}{stratum label}

\item{\code{dist}}{R6 class to define the enrollment distribution}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Design} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Design-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Design-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Design$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
