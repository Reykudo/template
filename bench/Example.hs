
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module Example where

import Data.Text

import Text.RawString.QQ

example1 :: Text
example1 =
  pack
    $ [r|<Ad sequence="${position}">
  <Wrapper>
    <AdSystem><![CDATA[${system_name}]]></AdSystem>
    <VASTAdTagURI><![CDATA[${url}]]></VASTAdTagURI>
    <Impression id="${system_name}"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=impression&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Impression>
    <Error><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=error&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}&errCode=[ERRORCODE]]]></Error>
    <Creatives>
      <Creative>
        <Linear>
          <TrackingEvents>
            <Tracking event="create"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=create&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="midpoint"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=midpoint&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="creativeView"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=creativeView&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="start"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=start&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="firstQuartile"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=firstQuartile&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="onVastLoad"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=onVastLoad&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="thirdQuartile"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=thirdQuartile&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="complete"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=complete&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="close"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=close&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="skip"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=skip&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="trueImpression"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=trueImpression&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="trueCreativeView"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=trueCreativeView&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="trueStart"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=trueStart&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="mute"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=mute&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="unmute"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=unmute&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="pause"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=pause&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="rewind"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=rewind&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="resume"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=resume&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="expand"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=expand&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="collapse"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=collapse&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
            <Tracking event="acceptInvitation"><![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=acceptInvitation&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]></Tracking>
          </TrackingEvents>
          <VideoClicks>
            <ClickTracking>
              <![CDATA[${scheme}://${host}/v1/${consumer}/${platform}/tracking?e=click&eid=${eventId}&st=${enumSourceType}&sn=${base64SourceName}&cid=${clientId}&placetype=${placeType}&adId=${adId}]]>
            </ClickTracking>
          </VideoClicks>
        </Linear>
      </Creative>
    </Creatives>
  </Wrapper>
</Ad>|]