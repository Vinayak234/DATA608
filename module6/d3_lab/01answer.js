d3.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module6/d3_lab/ue_industry.csv', data => {

  const xScaleLine = d3.scaleLinear()
  .domain(d3.extent(data, d => +d.index))
  .range([1180,20]);

  const yScaleLine = d3.scaleLinear()
  .domain(d3.extent(data, d => +d.Agriculture))
  .range([580,20]);

  let line4 = d3.line()
  .x(d => xScaleLine(d.index))
  .y(d => yScaleLine(d.Agriculture));

    // Define your scales and generator here.

    d3.select('#answer1')
    .append('path')
    .attr('d', line4(data))
    .attr('stroke', '#2e2928')
        // append more elements here

});
