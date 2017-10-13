/**
 * java.net.UnknownHostException: smtp.gmail.com
 *
 * Some hit the UnknownHostException: smtp.gmail.com, try ping smtp.gmail.com and make sure you
 * got a response (able to access). Often times, your connection may block by your firewall or
 * proxy behind.
 */

import java.util.Properties;

import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;

public class SendMailSSL {

    public static void main(String[] args) {
        final String to =       "aleksiarts@gmail.com";
        final String from =     "etherialsilence@gmail.com";
        final String host =     "smtp.gmail.com";
        final String port =     "465";
        final String username = "etherialsilence@gmail.com";
        final String password = "incursion15";

        /* Set up properties. */
        Properties props = new Properties();
        props.put("mail.smtp.auth", "true");
        props.put("mail.smtp.host", host);
        props.put("mail.smtp.port", port);
        props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
        props.put("mail.smtp.socketFactory.port", port);

        /* Get session object. */
        Session session = Session.getDefaultInstance(props,
            new javax.mail.Authenticator() {
                protected PasswordAuthentication getPasswordAuthentication() {
                    return new PasswordAuthentication(username, password);
                }
            });

        /* Create and send message. */
        try {
            Message message = new MimeMessage(session);
            message.setFrom(new InternetAddress(from));
            message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(to));
            message.setSubject("Test mail SSL");
            message.setText("This mail was sent via SSL.");
            Transport.send(message);
            System.out.println("Message sent successfully");
        } catch (MessagingException e) {
            throw new RuntimeException(e);
        }
    }
}
