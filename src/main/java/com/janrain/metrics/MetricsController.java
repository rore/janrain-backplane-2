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

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.AuthException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.*;

/**
 * @author Tom Raney
 */

@Controller
@RequestMapping(value = "/metrics/*")
public class MetricsController {

    private static final Logger logger = Logger.getLogger(MetricsController.class);

    @Inject
    private Backplane2Config bpConfig;

    @Inject
    private SuperSimpleDB superSimpleDb;

    @Inject
    private MetricsAccumulator metricsAccumulator;

    // - PUBLIC

    @RequestMapping(value = "/dump", method = RequestMethod.POST)
    public ResponseEntity<String> dump (@RequestBody MetricRequest metricRequest)
            throws SimpleDBException, BackplaneServerException, AuthException {

        bpConfig.checkMetricAuth(metricRequest.getUser(), metricRequest.getSecret());

        try {
            return new ResponseEntity<String>(
                    retrieveAllMetrics(),
                    new HttpHeaders() {{
                        add("Content-Type", "application/json");
                    }},
                    HttpStatus.OK);
        } catch (Exception e) {
            logger.error(e);
            throw new BackplaneServerException(e.getMessage());
        }
    }

    private String retrieveAllMetrics() throws SimpleDBException {

        // TODO fix broken code below - may only return partial results
        List<MetricMessage> metrics =
                superSimpleDb.retrieveSome(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_METRICS), MetricMessage.class).getLeft();

        StringBuilder sb = new StringBuilder().append("[");

        int count = 0;
        for (MetricMessage metric : metrics) {
            if (count++ > 0) {
                sb.append(",");
            }
            sb.append("{");
            sb.append("\"id\":\"" + metric.getIdValue() + "\",");
            sb.append("\"time_collected\":\"" + metric.get(MetricMessage.Field.TIME) + "\",");
            sb.append("\"metrics\":" + metric.get(MetricMessage.Field.PAYLOAD));
            sb.append("}");
        }

        sb.append("]");

        return sb.toString();

    }


}
