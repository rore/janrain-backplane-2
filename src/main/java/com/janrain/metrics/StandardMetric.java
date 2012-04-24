/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.metrics;

import com.yammer.metrics.core.HistogramMetric;
import com.yammer.metrics.core.MeterMetric;
import com.yammer.metrics.core.Metric;
import com.yammer.metrics.core.TimerMetric;

import java.io.Serializable;
import java.util.List;

/**
 * @author Tom Raney
 */
public class StandardMetric implements Serializable {

    public String name;
    public String unit;

    public double mean;
    public double median;
    public double max;
    public double min;
    public long count;

    public StandardMetric(String name) {
        this.name=name;
    }

    public StandardMetric(String name, Metric metric) {

        this.name = name;
        if ( metric instanceof MeterMetric) {
            unit = ((MeterMetric) metric).rateUnit().toString().toLowerCase();
            mean = ((MeterMetric) metric).meanRate();
            count = ((MeterMetric) metric).count();
        } else if ( metric instanceof HistogramMetric) {
            final double[] percentiles = ((HistogramMetric) metric).percentiles(0.5);
            mean = ((HistogramMetric) metric).mean();
            max = ((HistogramMetric) metric).max();
            min = ((HistogramMetric) metric).min();
            median = percentiles[0];
            count = ((HistogramMetric) metric).count();
        } else if ( metric instanceof TimerMetric) {
            final double[] percentiles = ((TimerMetric) metric).percentiles(0.5);
            unit = ((TimerMetric)metric).durationUnit().toString().toLowerCase();
            mean = ((TimerMetric) metric).mean();
            max = ((TimerMetric) metric).max();
            median = percentiles[0];
            count = ((TimerMetric) metric).count();
        }

    }

    public StandardMetric(String name, List<StandardMetric> metrics) {

        this.name = name;
        this.unit = metrics.get(0).unit;

        for (StandardMetric metric : metrics) {
            this.mean += metric.mean;
            this.median += metric.median;
            this.max = (metric.max > this.max ? metric.max : this.max);
            this.min = (metric.min < this.min ? metric.min : this.min);
            this.count += metric.count;
        }

        double size = (double)metrics.size();
        this.mean = this.mean/size;
        this.median = this.median/size;
    }

}
