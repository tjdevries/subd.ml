let channel_clear_event =
  {|
<h3 id="channel-chat-clear-event">Channel Chat Clear Event</h3>

<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Type</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code class="highlighter-rouge">broadcaster_user_id</code></td>
      <td>string</td>
      <td>The broadcaster user ID.</td>
    </tr>
    <tr>
      <td><code class="highlighter-rouge">broadcaster_user_name</code></td>
      <td>string</td>
      <td>The broadcaster display name.</td>
    </tr>
    <tr>
      <td><code class="highlighter-rouge">broadcaster_user_login</code></td>
      <td>string</td>
      <td>The broadcaster login.</td>
    </tr>
  </tbody>
</table>
|}
;;

let charity_donation_event =
  {|

<h3 id="charity-donation-event">Charity Donation Event</h3>

<p>Defines the data you receive in your event handler when users donate to the broadcaster’s charity campaign (see <a href="/docs/eventsub/eventsub-subscription-types#channelcharity_campaigndonate">channel.charity_campaign.donate</a>).</p>

<table>
  <thead>
    <tr>
      <th>Field</th>
      <th>Type</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code class="highlighter-rouge">id</code></td>
      <td>String</td>
      <td>An ID that identifies the donation. The ID is unique across campaigns.</td>
    </tr>
    <tr>
      <td><code class="highlighter-rouge">charity_logo</code></td>
      <td>String</td>
      <td>A URL to an image of the charity’s logo. The image’s type is PNG and its size is 100px X 100px.</td>
    </tr>
      <td><code class="highlighter-rouge">amount</code></td>
      <td>Object</td>
      <td>An object that contains the amount of money that the user donated.</td>
    </tr>
    <tr>
      <td>   <code class="highlighter-rouge">value</code></td>
      <td>Integer</td>
      <td>The monetary amount. The amount is specified in the currency’s minor unit. For example, the minor units for USD is cents, so if the amount is $5.50 USD, <code class="highlighter-rouge">value</code> is set to 550.</td>
    </tr>
    <tr>
      <td>   <code class="highlighter-rouge">decimal_places</code></td>
      <td>Integer</td>
      <td>The number of decimal places used by the currency. For example, USD uses two decimal places. Use this number to translate <code class="highlighter-rouge">value</code> from minor units to major units by using the formula:<br><br><code class="highlighter-rouge">value / 10^decimal_places</code></td>
    </tr>
    <tr>
      <td>   <code class="highlighter-rouge">currency</code></td>
      <td>String</td>
      <td>The ISO-4217 three-letter currency code that identifies the type of currency in <code class="highlighter-rouge">value</code>.</td>
    </tr>
  </tbody>
</table>

|}
;;
