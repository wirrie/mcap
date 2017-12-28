package ibAPI;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.ib.client.Bar;
import com.ib.client.CommissionReport;
import com.ib.client.Contract;
import com.ib.client.ContractDescription;
import com.ib.client.ContractDetails;
import com.ib.client.DeltaNeutralContract;
import com.ib.client.DepthMktDataDescription;
import com.ib.client.EClientSocket;
import com.ib.client.EJavaSignal;
import com.ib.client.EReader;
import com.ib.client.EReaderSignal;
import com.ib.client.EWrapper;
import com.ib.client.EWrapperMsgGenerator;
import com.ib.client.Execution;
import com.ib.client.FamilyCode;
import com.ib.client.HistogramEntry;
import com.ib.client.HistoricalTick;
import com.ib.client.HistoricalTickBidAsk;
import com.ib.client.HistoricalTickLast;
import com.ib.client.NewsProvider;
import com.ib.client.Order;
import com.ib.client.OrderState;
import com.ib.client.PriceIncrement;
import com.ib.client.SoftDollarTier;
import com.ib.client.TickAttr;

public class getData implements EWrapper{
	private static EClientSocket client = null;
	public EReaderSignal Signal;
	private EReader reader;
	
	public getData(){
		Signal = new EJavaSignal();
		client = new EClientSocket(this, Signal);
		client.eConnect("localhost", 7496, 0);
		client.startAPI();
		try {
			while (! (client.isConnected())){
				Thread.sleep (1000);
				System.out.println("Attempting to Connect");
			}
		} catch (Exception e) {
			System.out.println("Error connecting.");
		}
		System.out.println(client.isConnected());
		System.out.println("Connected");
		
		//Create a reader to consume messages from the TWS. The EReader will consume the incoming messages and put them in a queue
        //reader = new EReader(client, Signal);
        //reader.start();
        //Once the messages are in the queue, an additional thread can be created to fetch them

			reader = new EReader(client, Signal);   
	        
	        reader.start();
	        //An additional thread is created in this program design to empty the messaging queue
	        new Thread() {
	            public void run() {
	                while (client.isConnected()) {
	                    Signal.waitForSignal();
	                    try {
	                        reader.processMsgs();
	                    } catch (Exception e) {
	                        System.out.println("Exception: "+e.getMessage());
	                    }
	                }
	            }
	        }.start();
		  
	}
	
	static int min = -1;
	static int max = 99;
	public static void main(String[] args) throws IOException, InterruptedException, ParseException {
		getData d = new getData();
		fillArraysWithData();
		client.reqMarketDataType(2);
		Contract contract = new Contract();
		int id = 1000;
		long time_t = System.currentTimeMillis();
		//System.out.println(cusips.size());
		for (int x = 0; x<cusips.size(); x++){
			if (x > min && x < max){
				if ((System.currentTimeMillis() - time_t) > 1000 || x == 0){
					contract.symbol(cusips.get(x));
			        contract.secType("BOND");
			        contract.exchange("SMART");
			        contract.currency("USD");
			        System.out.println("Sending something: " + id);
			        client.reqMktData(id, contract, "", false, false, null);
			        //client.reqContractDetails(id, contract);
			        id++;
			        time_t = System.currentTimeMillis();
				}
				else {
					x--;
				}
			}
		}
		
		String expectedPattern = "MM/dd/yy";
		SimpleDateFormat formatter = new SimpleDateFormat(expectedPattern);
		Date todaysDate = Calendar.getInstance().getTime();
		
		
		System.out.println("parsing dates");
		
		ArrayList <Long> daysTillMaturity = new ArrayList <Long>();
		for (int x = 0; x>min && x<max; x++){
			Date date = formatter.parse(dates.get(x));
			System.out.println(date);
			long days = ChronoUnit.DAYS.between(todaysDate.toInstant(), date.toInstant());
			System.out.println(days);
			daysTillMaturity.add(days);
		}
		
		
		FileWriter fw = new FileWriter("DatesYields.txt");
		FileReader fr = new FileReader("DatesYields.txt");
		BufferedReader br = new BufferedReader(fr);
		String line;
		while ((line = br.readLine()) != null){
			fw.write(line+"\n");
		}
		for (int x = 0; x>min && x<max; x++){
			fw.write(daysTillMaturity.get(x) + "|" + yields.get(min+x+1)+"\n");
		}
		br.close();
		fw.close();
		client.eDisconnect();
	}
	
	static ArrayList <String> cusips = new ArrayList <String>();
	static ArrayList <String> dates = new ArrayList <String>();
	static ArrayList <Double> yields = new ArrayList <Double>();
	
	public static void fillArraysWithData() throws IOException, InterruptedException{
		BufferedReader br = new BufferedReader(new FileReader(new File("src/ibAPI/opdr112017.txt")));
		String line;
		int count = 0;
		while ((line = br.readLine()) != null){
			if (count > 7 && !line.contains("Treasury") && !line.contains("Total") && !line.contains("CUSIP")){
				cusips.add(line.split("      ")[0]);
				dates.add(line.split("   ")[6].replaceAll(" ", ""));
				yields.add(0.0);
			}
			count++;
		}
	}

	public void accountDownloadEnd(String arg0) {
		
	}

	public void accountSummary(int arg0, String arg1, String arg2, String arg3, String arg4) {
		
	}

	public void accountSummaryEnd(int arg0) {
		
	}

	public void accountUpdateMulti(int arg0, String arg1, String arg2, String arg3, String arg4, String arg5) {
		
	}

	public void accountUpdateMultiEnd(int arg0) {
		
	}

	public void bondContractDetails(int arg0, ContractDetails contract) {
		try {
			System.out.println("Details");
			Contract con = contract.contract();
			System.out.println(con.description());
		} catch (Exception e){
			System.out.println("Error getting contract info");
		}
	}

	public void commissionReport(CommissionReport arg0) {
		
	}

	public void connectAck() {
		
	}

	public void connectionClosed() {
		
	}

	public void contractDetails(int reqId, ContractDetails contractDetails) {
		System.out.println(EWrapperMsgGenerator.contractDetails(reqId, contractDetails)); 
	}

	public void contractDetailsEnd(int arg0) {
		try {
			System.out.println("ContractDetailsEnd");
		} 
		catch (Exception e){
			e.printStackTrace ();
	    }
	}

	public void currentTime(long arg0) {
		
	}

	public void deltaNeutralValidation(int arg0, DeltaNeutralContract arg1) {
		
	}

	public void displayGroupList(int arg0, String arg1) {
		
	}

	public void displayGroupUpdated(int arg0, String arg1) {
		
	}

	public void error(Exception arg0) {
		
	}

	public void error(String arg0) {
		
	}

	public void error(int arg0, int arg1, String arg2) {
		
	}

	public void execDetails(int arg0, Contract arg1, Execution arg2) {
		
	}

	public void execDetailsEnd(int arg0) {
		
	}

	public void fundamentalData(int arg0, String arg1) {
		
	}

	public void historicalData(int arg0, String arg1, double arg2, double arg3,
			double arg4, double arg5, int arg6, int arg7, double arg8,
			boolean arg9) {
		
	}

	public void managedAccounts(String arg0) {
		
	}

	public void marketDataType(int arg0, int arg1) {
		
	}

	public void nextValidId(int arg0) {

	}

	public void openOrder(int arg0, Contract arg1, Order arg2, OrderState arg3) {
		
	}

	public void openOrderEnd() {
		
	}

	public void orderStatus(int arg0, String arg1, double arg2, double arg3,
			double arg4, int arg5, int arg6, double arg7, int arg8, String arg9) {
		
	}

	public void position(String arg0, Contract arg1, double arg2, double arg3) {
		
	}

	public void positionEnd() {
		
	}

	public void positionMulti(int arg0, String arg1, String arg2,
			Contract arg3, double arg4, double arg5) {
		
	}

	public void positionMultiEnd(int arg0) {
		
	}

	public void realtimeBar(int arg0, long arg1, double arg2, double arg3,
			double arg4, double arg5, long arg6, double arg7, int arg8) {
		
	}

	public void receiveFA(int arg0, String arg1) {
		
	}

	public void scannerData(int arg0, int arg1, ContractDetails arg2,
			String arg3, String arg4, String arg5, String arg6) {
		
	}

	public void scannerDataEnd(int arg0) {
		
	}

	public void scannerParameters(String arg0) {
		
	}

	public void securityDefinitionOptionalParameterEnd(int arg0) {
		
	}

	public void softDollarTiers(int arg0, SoftDollarTier[] arg1) {
		
	}

	public void tickEFP(int arg0, int arg1, double arg2, String arg3,
			double arg4, int arg5, String arg6, double arg7, double arg8) {
		
	}

	public void tickGeneric(int tickerId, int tickType, double value) {
		System.out.println("Tick Generic. Ticker Id:" + tickerId + ", Field: " + tickType + ", Value: " + value);
	}

	public void tickOptionComputation(int arg0, int arg1, double arg2,
			double arg3, double arg4, double arg5, double arg6, double arg7,
			double arg8, double arg9) {
		
	}

	public void tickPrice(int tickerId, int field, double price, TickAttr attribs) {
		System.out.println("Tick Price. Ticker Id:"+tickerId+", Field: "+field+", Price: "+price+", CanAutoExecute: "+ attribs.canAutoExecute()
		        + ", pastLimit: " + attribs.pastLimit() + ", pre-open: " + attribs.preOpen() + " ");
		if (field == 51){
			yields.set(tickerId-1000, price);
		}
	}

	public void tickSize(int tickerId, int field, int size) {
		System.out.println("Tick Size. Ticker Id:" + tickerId + ", Field: " + field + ", Size: " + size);
	}

	public void tickSnapshotEnd(int arg0) {
		
	}

	public void tickString(int tickerId, int tickType, String value) {
		System.out.println("Tick string. Ticker Id:" + tickerId + ", Type: " + tickType + ", Value: " + value);
	}

	public void updateAccountTime(String arg0) {
		
	}

	public void updateAccountValue(String arg0, String arg1, String arg2,
			String arg3) {
		
	}

	public void updateMktDepth(int arg0, int arg1, int arg2, int arg3,
			double arg4, int arg5) {
		
	}

	public void updateMktDepthL2(int arg0, int arg1, String arg2, int arg3,
			int arg4, double arg5, int arg6) {
		
	}

	public void updateNewsBulletin(int arg0, int arg1, String arg2, String arg3) {
		
	}

	public void updatePortfolio(Contract arg0, double arg1, double arg2,
			double arg3, double arg4, double arg5, double arg6, String arg7) {
		
	}

	public void verifyAndAuthCompleted(boolean arg0, String arg1) {
		
	}

	public void verifyAndAuthMessageAPI(String arg0, String arg1) {
		
	}

	public void verifyCompleted(boolean arg0, String arg1) {
		
	}

	public void verifyMessageAPI(String arg0) {
		
	}


	@Override
	public void familyCodes(FamilyCode[] arg0) {
		
	}


	@Override
	public void headTimestamp(int arg0, String arg1) {
		
	}


	@Override
	public void histogramData(int arg0, List<HistogramEntry> arg1) {
		
	}


	@Override
	public void historicalData(int arg0, Bar arg1) {
		
	}


	@Override
	public void historicalDataEnd(int arg0, String arg1, String arg2) {
		
	}


	@Override
	public void historicalDataUpdate(int arg0, Bar arg1) {
		
	}


	@Override
	public void historicalNews(int arg0, String arg1, String arg2, String arg3,
			String arg4) {
		
	}


	@Override
	public void historicalNewsEnd(int arg0, boolean arg1) {
		
	}


	@Override
	public void historicalTicks(int arg0, List<HistoricalTick> arg1,
			boolean arg2) {
		
	}


	@Override
	public void historicalTicksBidAsk(int arg0,
			List<HistoricalTickBidAsk> arg1, boolean arg2) {
		
	}


	@Override
	public void historicalTicksLast(int arg0, List<HistoricalTickLast> arg1,
			boolean arg2) {
		
	}


	@Override
	public void marketRule(int arg0, PriceIncrement[] arg1) {
		
	}


	@Override
	public void mktDepthExchanges(DepthMktDataDescription[] arg0) {
		
	}


	@Override
	public void newsArticle(int arg0, int arg1, String arg2) {
		
	}


	@Override
	public void newsProviders(NewsProvider[] arg0) {
		
	}


	@Override
	public void orderStatus(int arg0, String arg1, double arg2, double arg3,
			double arg4, int arg5, int arg6, double arg7, int arg8,
			String arg9, double arg10) {
		
	}


	@Override
	public void pnl(int arg0, double arg1, double arg2, double arg3) {
		
	}


	@Override
	public void pnlSingle(int arg0, int arg1, double arg2, double arg3,
			double arg4, double arg5) {
		
	}


	@Override
	public void rerouteMktDataReq(int arg0, int arg1, String arg2) {
		
	}


	@Override
	public void rerouteMktDepthReq(int arg0, int arg1, String arg2) {
		
	}


	@Override
	public void securityDefinitionOptionalParameter(int arg0, String arg1,
			int arg2, String arg3, String arg4, Set<String> arg5,
			Set<Double> arg6) {
		
	}


	@Override
	public void smartComponents(int arg0,
			Map<Integer, Entry<String, Character>> arg1) {
		
	}


	@Override
	public void symbolSamples(int reqId, ContractDescription[] contractDescriptions) {
		System.out.println("Contract Descriptions. Request: " + reqId + "\n");
        for (ContractDescription  cd : contractDescriptions) {
            Contract c = cd.contract();
            StringBuilder derivativeSecTypesSB = new StringBuilder();
            for (String str : cd.derivativeSecTypes()) {
                derivativeSecTypesSB.append(str);
                derivativeSecTypesSB.append(",");
            }
            System.out.print("Contract. ConId: " + c.conid() + ", Symbol: " + c.symbol() + ", SecType: " + c.secType() + 
                    ", PrimaryExch: " + c.primaryExch() + ", Currency: " + c.currency() + 
                    ", DerivativeSecTypes:[" + derivativeSecTypesSB.toString() + "]");
            System.out.println();
        }
       
	}


	@Override
	public void tickNews(int arg0, long arg1, String arg2, String arg3,
			String arg4, String arg5) {
		
	}



	@Override
	public void tickReqParams(int tickerId, double minTick, String bboExchange, int snapshotPermissions) {
		System.out.println("Tick req params. Ticker Id:" + tickerId + ", Min tick: " + minTick + ", bbo exchange: " + bboExchange + ", Snapshot permissions: " + snapshotPermissions);
	}


}
