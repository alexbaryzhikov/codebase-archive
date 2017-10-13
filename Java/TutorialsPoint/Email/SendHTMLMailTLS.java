/**
 * javax.mail.AuthenticationFailedException: 534-5.7.14 Please log in via your web browser and then try again.
 *
 * Head over to Account Security Settings (https://www.google.com/settings/security/lesssecureapps)
 * and enable “Access for less secure apps”, this allows you to use the google smtp for clients
 * other than the official ones.
 */

import java.util.Properties;

import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;

public class SendHTMLMailTLS {

    public static void main(String[] args) {    
        final String to =       "aleksiarts@gmail.com";
        final String from =     "etherialsilence@gmail.com";
        final String host =     "smtp.gmail.com";
        final String port =     "587";
        final String username = "etherialsilence@gmail.com";
        final String password = "incursion15";

        /* Set up properties. */
        Properties props = new Properties();
        props.put("mail.smtp.auth", "true");
        props.put("mail.smtp.host", host);
        props.put("mail.smtp.port", port);
        props.put("mail.smtp.starttls.enable", "true");

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
            message.setSubject("Test HTML mail");
            message.setContent("<h1>This is HTML message</h1>", "text/html");
            Transport.send(message);
            System.out.println("Message sent successfully");
        } catch (MessagingException e) {
            throw new RuntimeException(e);
        }
    }
}
