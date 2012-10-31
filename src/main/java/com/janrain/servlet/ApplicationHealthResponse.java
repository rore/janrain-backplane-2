package com.janrain.servlet;

import java.util.HashMap;
import java.util.Map;

import com.janrain.backplane.server.config.Backplane1Config;

public class ApplicationHealthResponse {

	private String buildVersion;
	private String ec2InstanceId;
	private String bpInstanceId;
	private String readPingResponse;
	private String writePingResponse;
	private Map<String, String> writeRedisInfo = new HashMap<String, String>();
	private String status;

	public ApplicationHealthResponse(Backplane1Config bpConfig) {
		this.buildVersion = bpConfig.getBuildVersion();
		this.ec2InstanceId = Backplane1Config.getEC2InstanceId();
		this.bpInstanceId = bpConfig.getInstanceId();
	}

	public String getBuildVersion() {
		return buildVersion;
	}

	public String getEc2InstanceId() {
		return ec2InstanceId;
	}

	public String getBpInstanceId() {
		return bpInstanceId;
	}

	public void setReadPingResponse(String ping) {
		this.readPingResponse = ping;		
	}
	
	public void setWritePingResponse(String ping) {
		this.writePingResponse = ping;		
	}

	public String getReadPingResponse() {
		return readPingResponse;
	}

	public String getWritePingResponse() {
		return writePingResponse;
	}
	
	public Map<String, String> getWriteRedisInfo() {
		return writeRedisInfo;
	}

	public void setWriteRedisInfo(Map<String, String> info) {
		writeRedisInfo.putAll(info);
	}
	public Map<String, String> getReadRedisInfo() {
		return writeRedisInfo;
	}

	public void setReadRedisInfo(Map<String, String> info) {
		writeRedisInfo.putAll(info);
	}

	public void setSystemStatus(String status) {
		this.status = status;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

}
